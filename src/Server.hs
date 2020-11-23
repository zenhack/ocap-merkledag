{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Server
    ( StoreServer(..)
    ) where

import Zhp

import BlobStore

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage.Pure

import Supervisors (Supervisor)

import qualified Capnp.Rpc          as Rpc
import qualified Capnp.Rpc.Untyped  as RU
import qualified Capnp.Untyped.Pure as U

import           Control.Concurrent.STM      (atomically)
import           Control.Monad.STM.Class     (MonadSTM)
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

instance Store'server_ IO StoreServer (Maybe (U.Ptr)) where
    store'put = Rpc.pureHandler $
        \StoreServer{store, sup} Store'put'params{value} -> do
            (data_, ptrs) <- atomically $ runWriterT $ resolveCaps value
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

instance Ref'server_ IO RefServer (Maybe U.Ptr) where
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


type MonadAttachCaps m = MonadState [Hash] m

pullCap :: MonadAttachCaps m => m (Maybe Hash)
pullCap = do
    s <- get
    case s of
        [] -> pure Nothing
        x:xs -> do
            put xs
            pure $ Just x

traverseClientsMaybePtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> Maybe (U.Ptr) -> f (Maybe U.Ptr)
traverseClientsMaybePtr f p = do
    p' <- traverse (traverseClientsPtr f) p
    case p' of
        -- Normalize null clients to null pointers.
        Just (U.PtrCap c) | c == RU.nullClient -> pure Nothing
        _                                      -> pure p'

traverseClientsPtr :: Monad f => (Rpc.Client -> f Rpc.Client) -> U.Ptr -> f U.Ptr
traverseClientsPtr f = \case
    U.PtrList list  -> U.PtrList <$> traverseClientsList f list
    U.PtrStruct s   -> U.PtrStruct <$> traverseClientsStruct f s
    U.PtrCap client -> U.PtrCap <$> f client

traverseClientsList :: Monad f => (Rpc.Client -> f Rpc.Client) -> U.List -> f U.List
traverseClientsList f = \case
    U.ListPtr list -> U.ListPtr <$> traverse (traverseClientsMaybePtr f) list
    U.ListStruct list -> U.ListStruct <$> traverse (traverseClientsStruct f) list
    list -> pure list

traverseClientsStruct :: Monad f => (Rpc.Client -> f Rpc.Client) -> U.Struct -> f U.Struct
traverseClientsStruct f (U.Struct d (U.Slice ptrs)) =
    U.Struct d . U.Slice <$> traverse (traverseClientsMaybePtr f) ptrs

attachCaps :: MonadAttachCaps m => Maybe U.Ptr -> (Hash -> m Rpc.Client) -> m (Maybe U.Ptr)
attachCaps ptr mkClient = flip traverseClientsMaybePtr ptr $ \_ -> do
    c <- pullCap
    case c of
        Nothing -> pure RU.nullClient
        Just h  -> mkClient h

type MonadResolveCaps m = (MonadSTM m, MonadWriter [Hash] m)

resolveCaps :: MonadResolveCaps m => Maybe U.Ptr -> m (Maybe U.Ptr)
resolveCaps = traverseClientsMaybePtr resolveClient

resolveClient :: MonadResolveCaps m => Rpc.Client -> m Rpc.Client
resolveClient c = do
    c' <- Rpc.waitClient c
    case Rpc.unwrapServer c' of
        Just RefServer{hash} -> tell [hash] *> pure c'
        Nothing              -> pure RU.nullClient
