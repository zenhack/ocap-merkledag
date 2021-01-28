-- Low level data store that stores things in an in-memory map.
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlobStore.InMemory
    where

import           BlobStore
import qualified BlobStore.Raw            as Raw
import qualified Capnp
import           Capnp.Gen.Storage.Pure
import           Capnp.Rpc.Promise        (fulfill)
import qualified Capnp.Untyped.Pure       as U
import           Control.Concurrent.STM
import           Control.Exception.Safe   (impureThrow, throwM)
import           Control.Monad.Catch.Pure (runCatchT)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Functor.Identity    (runIdentity)
import qualified Data.Map.Strict          as M
import           Lifetimes
import           Zhp

newtype Store = Store (TVar StoreContents)

data StoreContents = StoreContents
    { blobs :: M.Map KnownHash BlobInfo
    , root  :: KnownHash
    }

data BlobInfo = BlobInfo
    { refCount :: !Int
    , bytes    :: LBS.ByteString
    }

handle :: Store -> Raw.Request -> IO ()
handle s@(Store var) req = do
    case req of
        Raw.GetRoot lt f -> do
            StoreContents{root} <- readTVarIO var
            acquire lt (acquireRef root s) >>= fulfill f

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
                modifyTVar var $ \sc -> decRef (root sc) (incRef h sc)
                fulfill f ()

        _ -> error "TODO"

acquireRef :: KnownHash -> Store -> Acquire KnownHash
acquireRef h (Store var) = do
    mkAcquire
        (atomically $ modifyTVar var $ incRef h)
        (const $ atomically $ modifyTVar var $ decRef h)
    pure h


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
