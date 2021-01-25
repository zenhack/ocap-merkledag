{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile
    where

import           BlobStore
import qualified BlobStore.BigFile.DiskTrie  as DiskTrie
import qualified BlobStore.BigFile.FileArena as FA
import qualified BlobStore.BigFile.MemTrie   as MemTrie
import           BlobStore.BigFile.SELock    hiding (new)
import qualified BlobStore.BigFile.SELock    as SELock
import qualified BlobStore.BigFile.TrieKey   as Key
import           Capnp.Gen.DiskBigfile.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Untyped.Pure          as U
import           Control.Concurrent.STM
import qualified Data.ByteArray              as BA
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Lifetimes                   (Acquire, withAcquire)
import           Zhp                         hiding (length)

open :: FilePath -> StoreInfo -> Acquire Store
open path StoreInfo{blobFile, bookKeepingFile=StoreInfo'bookKeepingFile'{arena}, blobs} = do
    blobArena <- openArena path blobFile
    spineArena <- openArena path arena
    liftIO $ atomically $ do
        checkPointLock <- SELock.new
        blobMap <- newTVar BlobMap
            { mem = MemTrie.empty
            , disk = blobs
            }
        pure Store
            { blobArena
            , spineArena
            , blobMap
            , checkPointLock
            }

openArena :: FilePath -> Arena -> Acquire (FA.FileArena a)
openArena rootPath Arena{path, size} =
    FA.open
        (rootPath <> "/" <> (T.unpack $ T.intercalate "/" $ V.toList path))
        (fromIntegral size)

type Leaf = StoredBlob (Maybe (U.Ptr))
type Branch = TrieMap'Branch BlobInfo

data Store = Store
    { blobArena      :: FA.FileArena Leaf
    , spineArena     :: FA.FileArena Branch
    , blobMap        :: TVar BlobMap
    , checkPointLock :: SELock
    }

data BlobMap = BlobMap
    { mem  :: MemTrie.MemTrie BlobInfo
    , disk :: TrieMap BlobInfo
    }

data CheckPointInfo = CheckPointInfo
    { blobsOffset :: Word64
    , spineOffset :: Word64
    }

atomicallyWrtCheckPoint :: Store -> IO a -> IO a
atomicallyWrtCheckPoint s io =
    withAcquire (acquireShared (checkPointLock s)) $ const io

unsafeGetCheckPointInfo :: Store -> IO CheckPointInfo
unsafeGetCheckPointInfo _s = undefined

checkPoint :: Store -> IO ()
checkPoint s = do
    info <- withAcquire (acquireExclusive (checkPointLock s)) $ \_ -> do
        checkPointBookkeeping s
    saveCheckPointInfo info

checkPointBookkeeping :: Store -> IO CheckPointInfo
checkPointBookkeeping s = join $ atomically $ do
    bm <- readTVar (blobMap s)
    _blobsOffset <- FA.getOffset (blobArena s)
    pure $ do
        (_newRoot, _toFree) <- MemTrie.mergeToDisk (mem bm) (disk bm) (spineArena s)
        atomically $ do
            _spineOffset <- FA.getOffset (spineArena s)
            error "TODO"

saveCheckPointInfo :: CheckPointInfo -> IO ()
saveCheckPointInfo _ = undefined

findBlob :: Store -> KnownHash -> IO (Maybe BlobInfo)
findBlob Store{spineArena, blobMap} (Sha256 h) = do
    BlobMap{mem, disk} <- atomically $ readTVar blobMap
    let key = case Key.makeKey (BA.convert h) of
            Nothing -> error "impossible"
            Just k  -> k
    case MemTrie.lookup key mem of
        Just info -> pure $ Just info
        Nothing   -> DiskTrie.lookup key disk spineArena

bigFilePutBlob :: Store -> KnownHash -> BS.ByteString -> IO ()
bigFilePutBlob s h bytes = do
    existing <- findBlob s h
    case existing of
        Just _ -> pure ()
        Nothing -> do
            off <- FA.writeBS bytes (blobArena s)
            let addr = Addr
                    { offset = fromIntegral off
                    , length = fromIntegral $ BS.length bytes
                    , flatMessage = True
                    }
            blobMapInsert (blobMap s) (spineArena s) addr

blobMapInsert :: TVar BlobMap -> FA.FileArena Branch -> Addr Leaf -> IO ()
blobMapInsert = undefined
