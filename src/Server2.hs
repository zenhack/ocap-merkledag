{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Server2
    ( StoreServer
    , acquireStoreServer
    ) where

import Zhp

import           BlobStore
import qualified BlobStore.HighLevel as HighLevel
import qualified BlobStore.Raw       as Raw
import qualified PutBytesStreaming

import           Capnp.Gen.Protocol.New
import           Capnp.Gen.Storage.New
import qualified Capnp.Gen.Util.New     as Util

import Control.Exception.Safe (SomeException, throwM, try)
import Control.Monad.ST       (RealWorld)
import Lifetimes
import Supervisors            (Supervisor)

import qualified Capnp.Message      as M
import qualified Capnp.New          as Capnp
import qualified Capnp.Rpc          as Rpc
import qualified Capnp.Rpc.Untyped  as RU
import qualified Capnp.Untyped      as U
import qualified Capnp.Untyped.Pure as PU

import Capnp            (ReadParam, WriteParam)
import Capnp.Classes    (FromStruct(fromStruct), ToStruct(toStruct))
import Capnp.Repr       (IsPtrRepr(toPtr))
import Capnp.Rpc.Errors (eFailed, wrapException)

import           Control.Concurrent.STM
import           Control.Monad.STM.Class
import qualified Data.ByteString         as BS
import           Data.Typeable           (Typeable, cast)
import qualified Data.Vector             as V

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

instance Capnp.SomeServer StoreServer

findByHash :: Supervisor -> Lifetime -> KnownHash -> Raw.Handler -> Rpc.Fulfiller (Capnp.Client (Ref (Maybe Capnp.AnyPointer))) -> STM ()
findByHash sup lt hash handler f = do
    hashFulfiller <- Rpc.newCallback $ \case
        Left e ->
            Rpc.breakPromise f e
        Right Nothing ->
            Rpc.breakPromise f $ eFailed "Not found"
        Right (Just v) ->
            Rpc.fulfill f =<< Capnp.export sup RefServer
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

instance Store'server_ (Maybe Capnp.AnyPointer) StoreServer where
    store'put srv@StoreServer{rawHandler, lifetime, sup} params result =
        Rpc.supervise sup $ fulfillWith result $ do
            hashRes <- Capnp.evalLimitT Capnp.defaultLimit $ do
                value <- Capnp.readField #value params
                case Capnp.fromRaw value of
                    Just (U.PtrList (U.List8 list)) -> do
                        bytes <- U.rawBytes list
                        liftIO $ putBytes srv bytes
                    _ -> do
                        pureVal <- Capnp.parse value
                        liftIO $ putPtr srv pureVal
            ref <- Capnp.export @(Ref (Maybe Capnp.AnyPointer)) sup RefServer{hash = hashRes, rawHandler, sup, lifetime}
            hash <- encodeHash <$> mustGetResource hashRes
            struct <- Capnp.createPure maxBound $ do
                msg <- Capnp.newMessage Nothing
                toStruct <$> Capnp.encode msg Store'put'results { hash, ref }
            Capnp.evalLimitT maxBound $ fromStruct struct

    store'findByHash StoreServer{rawHandler,sup,lifetime} =
        Capnp.handleParsed $ \Store'findByHash'params{hash} ->
            atomically $ case decodeHash hash of
                Left e -> throwM e
                Right h -> do
                    (p, f) <- Rpc.newPromiseClient
                    findByHash sup lifetime h rawHandler f
                    pure Store'findByHash'results{ref = p}

    store'putBytesStreaming srv@StoreServer{sup} = Capnp.handleParsed $ \_ -> do
        (stream, ref) <- PutBytesStreaming.makeStream sup (putBlobTree srv)
        pure Store'putBytesStreaming'results {stream, ref}

    store'subStore _ = Capnp.methodUnimplemented

    store'root srv@StoreServer{sup} = Capnp.handleParsed $ \_ -> do
        root <- Capnp.export sup (RootServer srv)
        pure Store'root'results{root}

putBlobTree :: forall a. Capnp.TypeParam a => StoreServer -> Capnp.Parsed a -> IO (Capnp.Client (Ref a))
putBlobTree srv@StoreServer{rawHandler, lifetime, sup} bt = do
    rawPtr <- Capnp.Raw <$> Capnp.createPure maxBound (do
        Capnp.Raw s <- Capnp.parsedToRaw bt
        pure (toPtr s))
    ptr <- Capnp.evalLimitT maxBound $ Capnp.parse rawPtr
    hash <- putPtr srv ptr
    Capnp.export @(Ref a) sup RefServer{hash, rawHandler, lifetime, sup}

-- TODO: put this somewhere more sensible.
castClient :: (Rpc.IsClient a, Rpc.IsClient b) => a -> b
castClient = Rpc.fromClient . Rpc.toClient

putBytes :: StoreServer -> BS.ByteString -> IO (Resource KnownHash)
putBytes StoreServer{lifetime, rawHandler} bytes = do
    (p, f) <- Rpc.newPromise
    (hash, msg) <- HighLevel.encodeBytes bytes
    let req = Raw.PutRequest
            { msg
            , hash
            , refs = []
            , result = f
            , lifetime
            }
    atomically $ rawHandler $ Raw.Put req
    Rpc.wait p

putPtr :: StoreServer -> Capnp.Parsed (Maybe Capnp.AnyPointer) -> IO (Resource KnownHash)
putPtr StoreServer{lifetime, rawHandler} ptr = do
    (hash, msg, refs) <- HighLevel.encodeBlob ptr resolveClient
    (p, f) <- Rpc.newPromise
    let req = Raw.PutRequest
            { msg = Capnp.msgToLBS msg
            , hash
            , refs
            , result = f
            , lifetime
            }
    atomically $ rawHandler $ Raw.Put req
    Rpc.wait p

instance Capnp.SomeServer RefServer where
    shutdown RefServer{hash} = releaseEarly hash
    unwrap = cast

instance Ref'server_ (Maybe Capnp.AnyPointer) RefServer where
    ref'get RefServer{hash, rawHandler, sup, lifetime} _params result =
        Rpc.supervise sup $ fulfillWith result $ do
            (p, f) <- Rpc.newPromise
            atomically $ rawHandler $ Raw.ReadRef hash f
            msg <- Rpc.wait p
            blob <- Capnp.msgToRaw @(StoredBlob (Maybe Capnp.AnyPointer)) msg
            ptrs <- Capnp.evalLimitT maxBound $ Capnp.parseField #ptrs blob
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

resolveClient :: (Monad m, MonadSTM m) => Rpc.Client -> m (Maybe (KnownHash, Rpc.Client))
resolveClient c = do
    c' <- Rpc.waitClient c
    case Rpc.unwrapServer c' of
        Just RefServer{hash} -> do
            res <- liftSTM $ mustGetResource hash
            pure $ Just (res, c')
        Nothing              -> pure Nothing

newtype RootServer = RootServer
    { storeSrv :: StoreServer
    }

instance Capnp.SomeServer RootServer

getRoot :: RootServer -> IO (Capnp.Client (Ref (Maybe Capnp.AnyPointer)))
getRoot RootServer{storeSrv=StoreServer{lifetime, sup, rawHandler}} = do
    (p, f) <- Rpc.newPromise
    atomically $ rawHandler $ Raw.GetRoot lifetime f
    hash <- Rpc.wait p
    Capnp.export sup RefServer { hash, rawHandler, sup, lifetime }

instance Util.Assignable'Getter'server_ (Ref (Maybe Capnp.AnyPointer)) RootServer where
    assignable'Getter'get srv = Capnp.handleParsed $ \_ -> do
        value <- getRoot srv
        pure Util.Assignable'Getter'get'results { value }

    assignable'Getter'subscribe _ = Capnp.methodUnimplemented

instance Util.Assignable'server_ (Ref (Maybe Capnp.AnyPointer)) RootServer where
    assignable'get srv = Capnp.handleParsed $ \_ -> do
        value <- getRoot srv
        pure Util.Assignable'get'results
            { value
            , setter = Rpc.fromClient RU.nullClient
            }

    assignable'asGetter srv@RootServer{storeSrv=StoreServer{sup}} = Capnp.handleParsed $ \_ -> do
        getter <- Capnp.export sup srv
        pure Util.Assignable'asGetter'results { getter }

    assignable'asSetter srv@(RootServer StoreServer{sup}) = Capnp.handleParsed $ \_ -> do
        setter <- Capnp.export sup srv
        pure Util.Assignable'asSetter'results { setter }

instance Util.Assignable'Setter'server_ (Ref (Maybe Capnp.AnyPointer)) RootServer where
    assignable'Setter'set RootServer{storeSrv = StoreServer{rawHandler}} =
        Capnp.handleParsed $ \Util.Assignable'Setter'set'params{value} -> do
            client <- Rpc.waitClient value
            (p, f) <- Rpc.newPromise
            case Rpc.unwrapServer client of
                Nothing -> throwM $ eFailed "Not a ref"
                Just RefServer{hash} -> atomically $ do
                    hash' <- mustGetResource hash
                    rawHandler $ Raw.SetRoot hash' f
            Rpc.wait p
            pure Capnp.def
