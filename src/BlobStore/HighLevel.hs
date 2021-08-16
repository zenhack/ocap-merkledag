{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module BlobStore.HighLevel
    ( encodeBlob
    , encodeBytes
    ) where

import           BlobStore
import           CanonicalizeBytes
import           Capnp.Gen.Storage.New
import qualified Capnp.Message             as M
import qualified Capnp.New                 as Capnp
import qualified Capnp.Rpc                 as Rpc
import qualified Capnp.Rpc.Untyped         as RU
import           Control.Monad.Catch       (MonadThrow(throwM))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer      (MonadWriter(tell), runWriterT)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Vector               as V
import           Zhp

encodeBlob
    :: MonadThrow m
    => Maybe (Parsed Capnp.AnyPointer)
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
        Capnp.Raw rawBlob <- Capnp.encode @(StoredBlob (Maybe Capnp.AnyPointer)) msg StoredBlob
            { data_
            , ptrs = V.fromList ptrs
            }
        (_, seg) <- Capnp.canonicalize rawBlob
        pure seg
    let bytes = Capnp.toByteString seg
        digest = computeHash bytes
    pure (digest, M.singleSegment seg, refs)

encodeBytes :: MonadThrow m => BS.ByteString -> m (KnownHash, LBS.ByteString)
encodeBytes bytes = do
    let lbs = canonicalizeBytesBlob bytes
        hash = computeHashLazy $ LBS.drop 8 lbs
    pure (hash, lbs)

traverseClientsMaybePtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> Maybe (Parsed Capnp.AnyPointer) -> f (Maybe (Parsed Capnp.AnyPointer))
traverseClientsMaybePtr _ Nothing = pure Nothing
traverseClientsMaybePtr f (Just p) = do
    p' <- traverseClientsPtr f p
    case p' of
        -- Normalize null clients to null pointers.
        Capnp.PtrCap c | c == RU.nullClient -> pure Nothing
        _                                   -> pure (Just p')

traverseClientsPtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> Parsed Capnp.AnyPointer -> f (Parsed Capnp.AnyPointer)
traverseClientsPtr f = \case
    Capnp.PtrList list  -> Capnp.PtrList <$> traverseClientsList f list
    Capnp.PtrStruct s   -> Capnp.PtrStruct <$> traverseClientsStruct f s
    Capnp.PtrCap client -> Capnp.PtrCap <$> f client

traverseClientsList :: Monad f => (Rpc.Client -> f Rpc.Client) -> Parsed Capnp.AnyList -> f (Parsed Capnp.AnyList)
traverseClientsList f = \case
    Capnp.ListPtr list -> Capnp.ListPtr <$> traverse (traverseClientsMaybePtr f) list
    Capnp.ListStruct list -> Capnp.ListStruct <$> traverse (traverseClientsStruct f) list
    list -> pure list

traverseClientsStruct :: Monad f => (Rpc.Client -> f Rpc.Client) -> Parsed Capnp.AnyStruct -> f (Parsed Capnp.AnyStruct)
traverseClientsStruct f (Capnp.Struct d ptrs) =
    Capnp.Struct d  <$> traverse (traverseClientsMaybePtr f) ptrs
