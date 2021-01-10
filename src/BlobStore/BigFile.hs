{-# LANGUAGE NamedFieldPuns #-}
module BlobStore.BigFile
    (
    ) where

import qualified BlobStore.BigFile.FileArena as FA
import           Capnp
    (defaultLimit, evalLimitT, msgToValue)
import           Capnp.Bits                  (ByteCount(..))
import           Capnp.Gen.DiskBigfile.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Untyped.Pure          as U
import           Data.Acquire                (Acquire)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Zhp

open :: FilePath -> StoreInfo -> Acquire Store
open path StoreInfo{blobFile, mapFile=StoreInfo'mapFile'{arena, rootAddr}} = do
    blobArena <- openArena path blobFile
    spineArena <- openArena path arena
    liftIO $ do
        rootMsg <- FA.readMsg rootAddr spineArena
        mapRoot <- evalLimitT defaultLimit $ msgToValue rootMsg
        pure Store
            { blobArena
            , spineArena
            , mapRoot
            }

openArena :: FilePath -> Arena -> Acquire FA.FileArena
openArena rootPath Arena{path, size} =
    FA.open
        (rootPath <> "/" <> (T.unpack $ T.intercalate "/" $ V.toList path))
        (fromIntegral size)

type Leaf = StoredBlob (Maybe (U.Ptr))
type Branch = TriePtr Leaf

data Store = Store
    { blobArena  :: FA.FileArena
    , spineArena :: FA.FileArena
    , mapRoot    :: Branch
    }
