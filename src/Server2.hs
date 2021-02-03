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

import Control.Exception.Safe (throwIO, throwM)
import Lifetimes
import Supervisors            (Supervisor)

import qualified Capnp
import qualified Capnp.Rpc          as Rpc
import qualified Capnp.Rpc.Untyped  as RU
import qualified Capnp.Untyped      as U
import qualified Capnp.Untyped.Pure as PU

import Capnp.Classes    (ToPtr(toPtr))
import Capnp.Rpc.Errors (eFailed)

import           Control.Concurrent.STM
import           Control.Monad.STM.Class
import           Control.Monad.State         (MonadState, evalStateT, get, put)
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


instance Store'server_ IO StoreServer (Maybe PU.Ptr) where
    store'put = Rpc.pureHandler $
        \StoreServer{store, sup} Store'put'params{value} -> do
            hash <- putPtr store value
            ref <- export_Ref sup RefServer{hash, store, sup}
            pure Store'put'results{hash, ref}

    store'findByHash = Rpc.pureHandler $
        \StoreServer{rawHandler,sup,lifetime} Store'findByHash'params{hash} ->
            atomically $ do
                (p, f) <- Rpc.newPromiseClient
                findByHash sup lifetime (decodeHash hash) rawHandler f
                pure Store'findByHash'results{ref = p}

    store'putBytesStreaming = Rpc.pureHandler $
        \srv@StoreServer{sup} _ -> do
            (stream, ref) <- PutBytesStreaming.makeStream sup (putBlobTree srv)
            pure Store'putBytesStreaming'results {stream, ref}

    store'subStore _ = Rpc.methodUnimplemented

putBlobTree :: StoreServer -> BlobTree -> IO (Ref BlobTree)
putBlobTree StoreServer{sup, store} bt = do
    rawPtr <- Capnp.createPure maxBound $ do
        msg <- Capnp.newMessage Nothing
        rawBt <- Capnp.cerialize msg bt
        toPtr msg rawBt
    ptr <- Capnp.evalLimitT maxBound $ Capnp.decerialize rawPtr
    hash <- putPtr store ptr
    castClient <$> export_Ref sup RefServer{hash, store, sup}

-- TODO: put this somewhere more sensible.
castClient :: (Rpc.IsClient a, Rpc.IsClient b) => a -> b
castClient = Rpc.fromClient . Rpc.toClient

putPtr :: BlobStore IO -> Maybe PU.Ptr -> IO Hash
putPtr store value = do
    (data_, ptrs) <- atomically $ runWriterT $ resolveCaps value
    putBlob store StoredBlob { data_, ptrs = V.fromList ptrs }

instance Rpc.Server IO RefServer where
    shutdown RefServer{hash} = releaseEarly hash
    unwrap = cast

instance Ref'server_ IO RefServer (Maybe PU.Ptr) where
    ref'get = Rpc.rawAsyncHandler $
        \RefServer{hash, rawHandler, sup, lifetime} _params result ->
            Rpc.supervise sup $ do
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
                error "TODO"


type MonadAttachCaps m = MonadState [Hash] m

pullCap :: MonadAttachCaps m => m (Maybe Hash)
pullCap = do
    s <- get
    case s of
        [] -> pure Nothing
        x:xs -> do
            put xs
            pure $ Just x

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

attachCaps :: MonadAttachCaps m => Maybe PU.Ptr -> (Hash -> m Rpc.Client) -> m (Maybe PU.Ptr)
attachCaps ptr mkClient = flip traverseClientsMaybePtr ptr $ \_ -> do
    c <- pullCap
    case c of
        Nothing -> pure RU.nullClient
        Just h  -> mkClient h

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
