-- Low level data store that stores things in an in-memory map.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module BlobStore.InMemory
    ( acquireHandler
    ) where

import           BlobStore
import qualified BlobStore.HighLevel      as HighLevel
import qualified BlobStore.Raw            as Raw
import qualified Capnp
import           Capnp.Gen.Storage.Pure
import           Capnp.Rpc.Errors         (eDisconnected)
import           Capnp.Rpc.Promise        (breakPromise, fulfill)
import qualified Capnp.Untyped.Pure       as U
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Exception.Safe   (impureThrow, throwM)
import           Control.Monad.Catch.Pure (runCatchT)
import           Control.Monad.State      (execState, modify)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Functor.Identity    (runIdentity)
import qualified Data.Map.Strict          as M
import           Lifetimes
import           Zhp

newtype Store = Store (TVar StoreContents)

data StoreContents = StoreContents
    { blobs :: !(M.Map KnownHash BlobInfo)
    , root  :: !KnownHash
    }

data BlobInfo = BlobInfo
    { refCount :: !Int
    , bytes    :: LBS.ByteString
    }

emptyBlob :: STM (KnownHash, LBS.ByteString)
emptyBlob = do
    (hash, msg, _) <- HighLevel.encodeBlob Nothing (const $ pure Nothing)
    pure (hash, Capnp.msgToLBS msg)

newStore :: STM Store
newStore = do
    (hash, bytes) <- emptyBlob
    Store <$> newTVar StoreContents
        { blobs = M.singleton hash BlobInfo { refCount = 1, bytes }
        , root = hash
        }

acquireHandler :: Acquire Raw.Handler
acquireHandler = do
    chan <- liftIO $ atomically newTChan
    s <- liftIO $ atomically newStore
    _ <- mkAcquire
        (Async.async $ forever $ atomically (readTChan chan) >>= handle s)
        Async.cancel
    pure (writeTChan chan)

handle :: Store -> Raw.Request -> IO ()
handle s@(Store var) req = do
    case req of
        Raw.GetRoot lt f -> do
            StoreContents{root} <- readTVarIO var
            ref <- acquire lt $ acquireRef root s
            fulfill f ref

        Raw.GetRef lt h f -> join $ atomically $ do
            StoreContents{blobs} <- readTVar var
            case M.lookup h blobs of
                Nothing -> do
                    fulfill f Nothing
                    pure $ pure ()
                Just _ -> pure $ do
                    r <- acquire lt (acquireRef h s)
                    fulfill f (Just r)

        Raw.SetRoot h f ->
            atomically $ do
                modifyTVar var $ \sc ->
                    let newRoot = h
                        oldRoot = root sc
                    in
                    flip execState sc $ do
                        modify $ incRef newRoot
                        modify $ decRef oldRoot
                        modify $ \sc' -> sc' { root = newRoot }
                fulfill f ()

        Raw.Checkpoint f ->
            fulfill f ()

        Raw.Put putReq ->
            handlePut s putReq

        Raw.ReadRef h f ->
            atomically $ do
                StoreContents{blobs} <- readTVar var
                res <- getResource h
                case res of
                    Nothing ->
                        breakPromise f eDisconnected
                    Just v -> do
                        let bs = LBS.toStrict $ bytes (blobs M.! v)
                        msg <- Capnp.bsToMsg bs
                        fulfill f msg

        Raw.SubscribeRoot _ ->
            error "TODO"


handlePut :: Store -> Raw.PutRequest -> IO ()
handlePut s@(Store var) Raw.PutRequest{hash, result, lifetime, refs, msg} =
    fulfill result =<< acquire lifetime (mkAcquire add remove)
  where
    add = atomically $ do
        sc <- readTVar var
        unless (hash `M.member` blobs sc) $ do
            let init = sc
                    { blobs = M.insert
                        hash
                        BlobInfo { bytes = msg, refCount = 0 }
                        (blobs sc)
                    }
            writeTVar var $! foldl' (flip incRef) init refs
        modifyTVar var $ incRef hash
        pure hash
    remove _ =
        releaseRef hash s


acquireRef :: KnownHash -> Store -> Acquire KnownHash
acquireRef h s@(Store var) = do
    mkAcquire
        (atomically $ modifyTVar var $ incRef h)
        (const $ releaseRef h s)
    pure h

releaseRef :: KnownHash -> Store -> IO ()
releaseRef h (Store var) =
    atomically $ modifyTVar var $ decRef h


incRef :: KnownHash -> StoreContents -> StoreContents
incRef h sc@StoreContents{blobs} =
    let info@BlobInfo{refCount} = blobs M.! h in
    sc { blobs = M.insert h info { refCount = refCount + 1 } blobs }

decRef :: KnownHash -> StoreContents -> StoreContents
decRef h sc@StoreContents{blobs} =
    let info@BlobInfo{refCount} = blobs M.! h
        refCount' = refCount - 1
    in
    if refCount' == 0 then
        dropBlob h sc
    else
        sc { blobs = M.insert h info { refCount = refCount' } blobs }

dropBlob :: KnownHash -> StoreContents -> StoreContents
dropBlob h sc@StoreContents{blobs} =
    let BlobInfo{bytes} = blobs M.! h
        blobs' = M.delete h blobs
    in
    let result = runIdentity $ runCatchT $ do
            StoredBlob{ptrs, data_ = _ :: Maybe U.Ptr} <- Capnp.lbsToValue bytes
            hashes <- for ptrs $ \ptr ->
                case decodeHash ptr of
                    Left e  -> throwM e
                    Right v -> pure v
            pure $ foldl'
                (flip decRef)
                sc { blobs = blobs' }
                hashes
    in
    case result of
        Left e  -> impureThrow e
        Right v -> v
