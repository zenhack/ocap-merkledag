{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlobStore.HighLevel
    ( encodeBlob
    , encodeBytes
    ) where

import           BlobStore
import           CanonicalizeBytes
import qualified Capnp
import           Capnp.Classes             (toStruct)
import           Capnp.Gen.Storage.New
import qualified Capnp.Message             as M
import qualified Capnp.Rpc                 as Rpc
import qualified Capnp.Rpc.Untyped         as RU
import qualified Capnp.Untyped.Pure        as PU
import           Control.Monad.Catch       (MonadThrow(throwM))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer      (MonadWriter(tell), runWriterT)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Vector               as V
import           Zhp

encodeBlob
    :: MonadThrow m
    => Maybe PU.Ptr
    -> (Rpc.Client -> m (Maybe (KnownHash, Rpc.Client)))
    -> m (KnownHash, Capnp.Message 'Capnp.Const, [KnownHash])
encodeBlob ptr resolveClient = do
    (data_, ptrs) <- runWriterT $ flip traverseClientsMaybePtr ptr $ \c ->
        lift (resolveClient c) >>= \case
            Nothing         -> pure RU.nullClient
            Just (hash, c') -> tell [encodeHash hash] *> pure c'
    refs <- case traverse decodeHash ptrs of
        Left e  -> throwM e
        Right v -> pure v
    seg :: Capnp.Segment 'Capnp.Const <- Capnp.createPure maxBound $ do
        msg <- Capnp.newMessage Nothing
        rawBlob <- Capnp.cerialize msg StoredBlob
            { data_
            , ptrs = V.fromList ptrs
            }
        (_, seg) <- Capnp.canonicalize (toStruct rawBlob)
        pure seg
    let bytes = Capnp.toByteString seg
        digest = computeHash bytes
    pure (digest, M.singleSegment seg, refs)

encodeBytes :: MonadThrow m => BS.ByteString -> m (KnownHash, LBS.ByteString)
encodeBytes bytes = do
    let lbs = canonicalizeBytesBlob bytes
        hash = computeHashLazy $ LBS.drop 8 lbs
    pure (hash, lbs)

traverseClientsMaybePtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> Maybe (PU.Ptr) -> f (Maybe PU.Ptr)
traverseClientsMaybePtr _ Nothing = pure Nothing
traverseClientsMaybePtr f (Just p) = do
    p' <- traverseClientsPtr f p
    case p' of
        -- Normalize null clients to null pointers.
        PU.PtrCap c | c == RU.nullClient -> pure Nothing
        _                                -> pure (Just p')

traverseClientsPtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> PU.Ptr -> f PU.Ptr
traverseClientsPtr f = \case
    PU.PtrList list  -> PU.PtrList <$> traverseClientsList f list
    PU.PtrStruct s   -> PU.PtrStruct <$> traverseClientsStruct f s
    PU.PtrCap client -> PU.PtrCap <$> f client

traverseClientsList :: Monad f => (Rpc.Client -> f Rpc.Client) -> PU.List -> f PU.List
traverseClientsList f = \case
    PU.ListPtr list -> PU.ListPtr <$> traverse (traverseClientsMaybePtr f) list
    PU.ListStruct list -> PU.ListStruct <$> traverse (traverseClientsStruct f) list
    list -> pure list

traverseClientsStruct :: Monad f => (Rpc.Client -> f Rpc.Client) -> PU.Struct -> f PU.Struct
traverseClientsStruct f (PU.Struct d (PU.Slice ptrs)) =
    PU.Struct d . PU.Slice <$> traverse (traverseClientsMaybePtr f) ptrs
