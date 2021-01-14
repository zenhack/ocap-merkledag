{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile
    (
    ) where

import           BlobStore
import qualified BlobStore.BigFile.DiskTrie  as DiskTrie
import qualified BlobStore.BigFile.FileArena as FA
import qualified BlobStore.BigFile.MemTrie   as MemTrie
import qualified BlobStore.BigFile.TrieKey   as Key
import           Capnp
    (defaultLimit, evalLimitT, msgToValue)
import           Capnp.Bits                  (ByteCount(..))
import           Capnp.Gen.DiskBigfile.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Untyped.Pure          as U
import           Control.Concurrent.STM
import           Data.Acquire                (Acquire)
import qualified Data.ByteArray              as BA
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Zhp                         hiding (length)

open :: FilePath -> StoreInfo -> Acquire Store
open path StoreInfo{blobFile, mapFile=StoreInfo'mapFile'{arena, mapRoot}} = do
    blobArena <- openArena path blobFile
    spineArena <- openArena path arena
    blobMap <- liftIO $ newTVarIO BlobMap
        { mem = MemTrie.empty
        , disk = mapRoot
        }
    pure Store { blobArena, spineArena, blobMap }

openArena :: FilePath -> Arena -> Acquire (FA.FileArena a)
openArena rootPath Arena{path, size} =
    FA.open
        (rootPath <> "/" <> (T.unpack $ T.intercalate "/" $ V.toList path))
        (fromIntegral size)

type Leaf = StoredBlob (Maybe (U.Ptr))
type Branch = TrieBranch Leaf

data Store = Store
    { blobArena  :: FA.FileArena Leaf
    , spineArena :: FA.FileArena Branch
    , blobMap    :: TVar BlobMap
    }

data BlobMap = BlobMap
    { mem  :: MemTrie.MemTrie (Addr Leaf)
    , disk :: TriePtr Leaf
    }

findBlob :: Store -> KnownHash -> IO (Maybe (Addr Leaf))
findBlob Store{spineArena, blobMap} (Sha256 h) = do
    BlobMap{mem, disk} <- atomically $ readTVar blobMap
    let key = case Key.makeKey (BA.convert h) of
            Nothing -> error "impossible"
            Just k  -> k
    case MemTrie.lookup key mem of
        Just addr -> pure $ Just addr
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
