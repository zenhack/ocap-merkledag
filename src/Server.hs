{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Server where

import Zhp

import BlobStore

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage.Pure

import Supervisors (Supervisor)

import qualified Capnp.Rpc          as Rpc
import qualified Capnp.Rpc.Untyped  as RU
import qualified Capnp.Untyped.Pure as U

import           Control.Monad.State         (MonadState, evalStateT, get, put)
import           Control.Monad.Writer.Strict (MonadWriter, runWriterT, tell)
import           Data.Typeable               (Typeable, cast)
import qualified Data.Vector                 as V

data StoreServer = StoreServer
    { store :: BlobStore IO
    , sup   :: Supervisor
    }

data RefServer = RefServer
    { hash  :: Hash
    , store :: BlobStore IO
    , sup   :: Supervisor
    }
    deriving(Typeable)

instance Rpc.Server IO StoreServer

instance Store'server_ IO StoreServer where
    store'put = Rpc.pureHandler $
        \StoreServer{store, sup} Store'put'params{value} -> do
            (data_, ptrs) <- runWriterT $ resolveCaps value
            hash <- putBlob store StoredBlob { data_, ptrs = V.fromList ptrs }
            ref <- export_Ref sup RefServer{hash, store, sup}
            pure Store'put'results{hash, ref}

    store'findByHash = Rpc.pureHandler $
        \StoreServer{store, sup} Store'findByHash'params{hash} -> do
            -- Make sure the blob is present. TODO: maybe add a helper
            -- for the store that checks for existence without actually
            -- loading it, which is wasteful here.
            StoredBlob{} <- getBlob store hash

            ref <- export_Ref sup RefServer{hash, store, sup}
            pure Store'findByHash'results{ref}

    store'putBytesStreaming _ = Rpc.methodUnimplemented
    store'subStore _ = Rpc.methodUnimplemented

instance Rpc.Server IO RefServer where
    unwrap = cast

instance Ref'server_ IO RefServer where
    ref'get = Rpc.pureHandler $
        \RefServer{hash, store, sup} _ -> do
            StoredBlob{data_, ptrs} <- getBlob store hash
            value <- flip evalStateT (V.toList ptrs) $ attachCaps data_ $ \ptrHash ->
                RU.toClient <$> export_Ref sup RefServer
                    { hash = ptrHash
                    , store
                    , sup
                    }
            pure Ref'get'results { value }


type MonadAttachCaps m = MonadState [StoredClient] m

pullCap :: MonadAttachCaps m => m (Maybe Hash)
pullCap = do
    s <- get
    case s of
        [] -> pure Nothing
        StoredClient'hash x:xs -> do
            put xs
            pure $ Just x
        _ : xs -> do
            put xs
            pure Nothing

traverseClientsMaybePtr :: Applicative f => (Rpc.Client -> f Rpc.Client) -> Maybe (U.Ptr) -> f (Maybe U.Ptr)
traverseClientsMaybePtr f = traverse (traverseClientsPtr f)

traverseClientsPtr :: Applicative f => (Rpc.Client -> f Rpc.Client) -> U.Ptr -> f U.Ptr
traverseClientsPtr f = \case
    U.PtrList list -> U.PtrList <$> traverseClientsList f list
    U.PtrStruct s -> U.PtrStruct <$> traverseClientsStruct f s
    U.PtrCap client -> U.PtrCap <$> f client

traverseClientsList :: Applicative f => (Rpc.Client -> f Rpc.Client) -> U.List -> f U.List
traverseClientsList f = \case
    U.ListPtr list -> U.ListPtr <$> traverse (traverseClientsMaybePtr f) list
    U.ListStruct list -> U.ListStruct <$> traverse (traverseClientsStruct f) list
    list -> pure list

traverseClientsStruct :: Applicative f => (Rpc.Client -> f Rpc.Client) -> U.Struct -> f U.Struct
traverseClientsStruct f (U.Struct d (U.Slice ptrs)) =
    U.Struct d . U.Slice <$> traverse (traverseClientsMaybePtr f) ptrs

attachCaps :: MonadAttachCaps m => Maybe U.Ptr -> (Hash -> m Rpc.Client) -> m (Maybe U.Ptr)
attachCaps ptr mkClient = flip traverseClientsMaybePtr ptr $ \_ -> do
    c <- pullCap
    case c of
        Nothing -> pure RU.nullClient
        Just h  -> mkClient h

type MonadResolveCaps m = MonadWriter [StoredClient] m

resolveCaps :: MonadResolveCaps m => Maybe U.Ptr -> m (Maybe U.Ptr)
resolveCaps = traverseClientsMaybePtr resolveClient

resolveClient ::  MonadResolveCaps m => Rpc.Client -> m Rpc.Client
resolveClient c = do
    -- TODO: wait for the promise to resolve if needed
    case Rpc.unwrapServer c of
        Just RefServer{hash} -> tell [StoredClient'hash hash] *> pure c
        Nothing              -> tell [StoredClient'null] *> pure RU.nullClient
