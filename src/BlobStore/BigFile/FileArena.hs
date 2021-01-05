module BlobStore.BigFile.FileArena
    ( FileArena
    , fromFd
    ) where

import Capnp.Bits                 (ByteCount)
import Capnp.Gen.DiskBigfile.Pure
import Capnp.Message              (singleSegment)

import qualified Capnp
import qualified Data.ByteString            as BS
import qualified System.Posix.IO.ByteString as BIO


byteCountToOffset :: ByteCount -> FileOffset
byteCountToOffset = fromIntegral

data FileArena = FileArena
    { fd        :: Fd
    , nextAlloc :: TVar FileOffset
    }

fromFd :: Fd -> FileOffset -> IO FileArena
fromFd fd off = do
    nextAlloc <- newTVar off
    pure FileArena { fd, nextAlloc }

alloc :: ByteCount -> FileArena -> STM FileOffset
alloc count FileArena{nextAlloc} = do
    offset <- readTVar nextAlloc
    writeTVar nextAlloc $! nextAlloc + count
    pure offset

writeBS :: BS.ByteString -> FileArena -> IO Offset
writeBS bs = writeLBS (LBS.fromStrict bs)

writeLBS :: LBS.ByteString -> FileArena -> IO Offset
writeLBS lbs arena = do
    offset <- atomically $ alloc $ ByteCount $ LBS.length lbs
    fdPwritev fd (LBS.chunks lbs) offset
    pure offset

-- TODO: replace this with a wrapper for the pwritev() syscall, maybe pushing one
-- upstream.
fdPwritev :: Fd -> [BS.ByteString] -> FileOffset -> IO ()
fdPwritev fd chunks offset = case chunks of
    [] -> pure ()
    (c:cs) -> do
        fdPwrite fd c offset
        fdPwritev fd cs (ofsfet + BS.length c)



----

readMsg :: Addr a -> FileArena -> IO (Capnp.Message 'Capnp.Const)
readMsg Addr{offset, length, flatMessage} = do
    bytes <- BIO.fdPread length offset
    if flatMessage
        then pure $ singleSegment $ Capnp.fromByteString bytes
        else Capnp.bsToMsg bytes

writeMsg :: Capnp.Message 'Capnp.Const -> FileArena -> IO (Addr a)
writeMsg msg arena = do
    nsegs <- Capnp.numSegs msg
    if nsegs == 1
        then (do
            seg <- getSegment msg 0
            writeSingleSegment seg arena)
        else
            writeMultiSegment msg arena

writeSingleSegment :: Capnp.Segment 'Capnp.Const -> FileArena -> IO (Addr a)
writeSingleSegment seg arena = do
    let bs = Capnp.toByteString seg
    offset <- writeBS bs arena
    pure Addr {offset, length = BS.length bs, flatMessage = True}

writeMultiSegment :: Capnp.Message 'Capnp.Const -> FileArena -> IO (Addr a)
writeMultiSegment msg arena = do
    let lbs = Capnp.msgToLBS msg
    offset <- writeLBS lbs arena
    pure Addr {offset, length = LBS.length lbs, flatMessage = False}
