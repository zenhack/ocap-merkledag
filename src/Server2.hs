{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Server2
    ( acquireStoreServer
    ) where

import Zhp

import           BlobStore
import qualified BlobStore.Raw     as Raw
import qualified PutBytesStreaming

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage       as RawStorage
import Capnp.Gen.Storage.Pure

import Control.Exception.Safe (SomeException, throwIO, throwM, try)
import Lifetimes
import Supervisors            (Supervisor)

import qualified Capnp
import qualified Capnp.Message      as M
import qualified Capnp.Rpc          as Rpc
import qualified Capnp.Rpc.Untyped  as RU
import qualified Capnp.Untyped      as U
import qualified Capnp.Untyped.Pure as PU

import Capnp.Classes
    (FromStruct(fromStruct), ToPtr(toPtr), ToStruct(toStruct))
import Capnp.Rpc.Errors (eFailed, wrapException)

import           Control.Concurrent.STM
import           Control.Monad.STM.Class
import           Control.Monad.Writer.Strict (MonadWriter, runWriterT, tell)
import           Data.Typeable               (Typeable, cast)
import qualified Data.Vector                 as V

acquireStoreServer :: Supervisor -> Raw.Handler -> Acquire StoreServer
acquireStoreServer sup h = do
    lifetime <- currentLifetime
    pure StoreServer
        { rawHandler = h
        , sup
        , lifetime
        }

data StoreServer = StoreServer
    { rawHandler :: Raw.Handler
    , sup        :: Supervisor
    , lifetime   :: Lifetime
    }

data RefServer = RefServer
    { hash       :: Resource KnownHash
    , rawHandler :: Raw.Handler
    , sup        :: Supervisor
    , lifetime   :: Lifetime
    }
    deriving(Typeable)

instance Rpc.Server IO StoreServer

findByHash :: Supervisor -> Lifetime -> KnownHash -> Raw.Handler -> Rpc.Fulfiller (Ref (Maybe PU.Ptr)) -> STM ()
findByHash sup lt hash handler f = do
    hashFulfiller <- Rpc.newCallback $ \case
        Left e ->
            Rpc.breakPromise f e
        Right Nothing ->
            Rpc.breakPromise f $ eFailed "Not found"
        Right (Just v) ->
            Rpc.fulfill f =<< export_Ref sup RefServer
                { hash = v
                , rawHandler = handler
                , sup = sup
                , lifetime = lt
                }
    handler $ Raw.GetRef lt hash hashFulfiller

fulfillWith :: Rpc.Fulfiller a -> IO a -> IO ()
fulfillWith f io =
    -- TODO: move this to haskell-capnp
    try io >>= \case
        Left (e :: SomeException) -> Rpc.breakPromise f (wrapException False e)
        Right v                   -> Rpc.fulfill f v

instance Store'server_ IO StoreServer (Maybe PU.Ptr) where
    store'put = Rpc.rawAsyncHandler $
        \srv@StoreServer{rawHandler, lifetime, sup} params result ->
            Rpc.supervise sup $ fulfillWith result $ do
                Store'put'params{value} <-
                    Capnp.evalLimitT Capnp.defaultLimit $ Capnp.decerialize params
                hashRes <- putPtr srv value
                ref <- export_Ref sup RefServer{hash = hashRes, rawHandler, sup, lifetime}
                hash <- encodeHash <$> mustGetResource hashRes
                struct <- Capnp.createPure maxBound $ do
                    msg <- Capnp.newMessage Nothing
                    toStruct <$> Capnp.cerialize msg Store'put'results { hash, ref }
                Capnp.evalLimitT maxBound $ fromStruct struct

    store'findByHash = Rpc.pureHandler $
        \StoreServer{rawHandler,sup,lifetime} Store'findByHash'params{hash} ->
            atomically $ case decodeHash hash of
                Left e -> throwM e
                Right h -> do
                    (p, f) <- Rpc.newPromiseClient
                    findByHash sup lifetime h rawHandler f
                    pure Store'findByHash'results{ref = p}

    store'putBytesStreaming = Rpc.pureHandler $
        \srv@StoreServer{sup} _ -> do
            (stream, ref) <- PutBytesStreaming.makeStream sup (putBlobTree srv)
            pure Store'putBytesStreaming'results {stream, ref}

    store'subStore _ = Rpc.methodUnimplemented

putBlobTree :: StoreServer -> BlobTree -> IO (Ref BlobTree)
putBlobTree srv@StoreServer{rawHandler, lifetime, sup} bt = do
    rawPtr <- Capnp.createPure maxBound $ do
        msg <- Capnp.newMessage Nothing
        rawBt <- Capnp.cerialize msg bt
        toPtr msg rawBt
    ptr <- Capnp.evalLimitT maxBound $ Capnp.decerialize rawPtr
    hash <- putPtr srv ptr
    castClient <$> export_Ref sup RefServer{hash, rawHandler, lifetime, sup}

-- TODO: put this somewhere more sensible.
castClient :: (Rpc.IsClient a, Rpc.IsClient b) => a -> b
castClient = Rpc.fromClient . Rpc.toClient

putPtr :: StoreServer -> Maybe PU.Ptr -> IO (Resource KnownHash)
putPtr StoreServer{lifetime, rawHandler}  value = do
    (data_, ptrs) <- atomically $ runWriterT $ resolveCaps value
    refs <- case traverse decodeHash ptrs of
        Left e  -> throwIO e
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
    let digest = computeHash bytes
    (p, f) <- Rpc.newPromise
    let req = Raw.PutRequest
            { msg = M.singleSegment seg
            , hash = digest
            , refs
            , result = f
            , lifetime
            }
    atomically $ rawHandler $ Raw.Put req
    Rpc.wait p

instance Rpc.Server IO RefServer where
    shutdown RefServer{hash} = releaseEarly hash
    unwrap = cast

instance Ref'server_ IO RefServer (Maybe PU.Ptr) where
    ref'get = Rpc.rawAsyncHandler $
        \RefServer{hash, rawHandler, sup, lifetime} _params result ->
            Rpc.supervise sup $ fulfillWith result $ do
                (p, f) <- Rpc.newPromise
                atomically $ rawHandler $ Raw.ReadRef hash f
                msg <- Rpc.wait p
                blob :: RawStorage.StoredBlob (Maybe (U.Ptr 'Capnp.Const)) 'Capnp.Const <- Capnp.msgToValue msg
                (ptrs :: V.Vector Hash) <- Capnp.evalLimitT maxBound $
                    RawStorage.get_StoredBlob'ptrs blob >>= Capnp.decerialize
                clients <- atomically $
                    for ptrs $ \ptr -> do
                        case decodeHash ptr of
                            Left e -> throwM e
                            Right hash -> do
                                (p, f) <- Rpc.newPromiseClient
                                findByHash sup lifetime hash rawHandler f
                                pure $ Rpc.toClient p
                Capnp.evalLimitT maxBound $
                    -- Attach the cap table and cast from StoredBlob(T) to the results.
                    -- The latter is to get around the fact that  the haskell-capnp API
                    -- makes it harder to extend an immutable message than it should be.
                    -- For now, we hack around this by exploting the fact that
                    -- `(value :T)` will serendipitously allocate the pointer in the
                    -- same place as `StoredBlob(T)`
                    U.tMsg (pure . M.withCapTable clients) (toStruct blob)
                    >>= fromStruct . toStruct

traverseClientsMaybePtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> Maybe (PU.Ptr) -> f (Maybe PU.Ptr)
traverseClientsMaybePtr f p = do
    p' <- traverse (traverseClientsPtr f) p
    case p' of
        -- Normalize null clients to null pointers.
        Just (PU.PtrCap c) | c == RU.nullClient -> pure Nothing
        _                                       -> pure p'

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

type MonadResolveCaps m = (MonadSTM m, MonadWriter [Hash] m)

resolveCaps :: MonadResolveCaps m => Maybe PU.Ptr -> m (Maybe PU.Ptr)
resolveCaps = traverseClientsMaybePtr resolveClient

resolveClient :: MonadResolveCaps m => Rpc.Client -> m Rpc.Client
resolveClient c = do
    c' <- Rpc.waitClient c
    case Rpc.unwrapServer c' of
        Just RefServer{hash} -> do
            res <- liftSTM $ mustGetResource hash
            tell [encodeHash res] *> pure c'
        Nothing              -> pure RU.nullClient
