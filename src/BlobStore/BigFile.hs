module BlobStore.BigFile
    (
    ) where

import           Capnp.Bits                 (ByteCount (..))
import           Capnp.Gen.DiskBigfile.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Untyped.Pure         as U

open :: FilePath -> StoreInfo -> IO Store

close :: Store -> IO ()

type Leaf = StoredBlob (Maybe (U.Ptr))
type Branch = TriePtr Leaf

data Store = Store
    { blobArena  :: FileArena Leaf
    , spineArena :: FileArena Branch
    , mapRoot    :: Branch
    }

data FileArena a = FileArena a
    { fd        :: Fd
    , nextAlloc :: TVar Offset
    }

alloc :: ByteCount -> Store -> STM Offset
alloc count Store{nextAlloc} = do
    off@(Offset addr) <- readTVar nextAlloc
    writeTVar nextAlloc $! addr + count
    pure off
