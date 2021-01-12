{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile
    (
    ) where

import           BlobStore
import qualified BlobStore.BigFile.FileArena as FA
import qualified BlobStore.BigFile.MemTrie   as MemTrie
import           Capnp
    (defaultLimit, evalLimitT, msgToValue)
import           Capnp.Bits                  (ByteCount(..))
import           Capnp.Gen.DiskBigfile.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Untyped.Pure          as U
import           Control.Concurrent.STM      (atomically)
import           Data.Acquire                (Acquire)
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Zhp                         hiding (length)

open :: FilePath -> StoreInfo -> Acquire Store
open path StoreInfo{blobFile, mapFile=StoreInfo'mapFile'{arena, mapRoot}} = do
    blobArena <- openArena path blobFile
    spineArena <- openArena path arena
    pure Store
        { blobArena
        , spineArena
        , blobMap = BlobMap
            { mem = MemTrie.empty
            , disk = mapRoot
            }
        }

openArena :: FilePath -> Arena -> Acquire (FA.FileArena a)
openArena rootPath Arena{path, size} =
    FA.open
        (rootPath <> "/" <> (T.unpack $ T.intercalate "/" $ V.toList path))
        (fromIntegral size)

type Leaf = StoredBlob (Maybe (U.Ptr))
type Branch = TriePtr Leaf

data Store = Store
    { blobArena  :: FA.FileArena Leaf
    , spineArena :: FA.FileArena Branch
    , blobMap    :: BlobMap
    }

data BlobMap = BlobMap
    { mem  :: MemTrie.MemTrie (Addr (Maybe U.Ptr))
    , disk :: TriePtr (StoredBlob (Maybe U.Ptr))
    }
{-

findBlob :: Store -> KnownHash -> IO (Maybe (Addr a))
findBlob = undefined

bigFilePutBlob :: Store -> KnownHash -> BS.ByteString -> IO ()
bigFilePutBlob s h bytes = do
    -- TODO: do we need to check for an existing blob first?
    off <- FA.writeBS bytes (blobArena s)
    let addr = Addr
            { offset = fromIntegral off
            , length = fromIntegral $ BS.length bytes
            , flatMessage = True
            }
    atomically $ M.insert addr h (addrCache s)
    pure ()

trieInsert = undefined
-}
