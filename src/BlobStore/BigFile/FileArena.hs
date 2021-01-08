{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
module BlobStore.BigFile.FileArena
    ( FileArena
    , fromFd
    , open
    , readMsg
    , writeMsg
    ) where

import Zhp hiding (length)

import Capnp.Bits                 (ByteCount)
import Capnp.Gen.DiskBigfile.Pure
import Control.Concurrent.STM
import Control.Exception.Safe     (throwIO)
import Data.Acquire               (Acquire)
import System.Posix.Types         (Fd, FileOffset)

import qualified Capnp
import qualified Capnp.Message        as M
import qualified Data.Acquire         as Acquire
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified System.Posix.IO      as PIO
import qualified Unix


byteCountToOffset :: ByteCount -> FileOffset
byteCountToOffset = fromIntegral

data FileArena = FileArena
    { fd        :: Fd
    , nextAlloc :: TVar FileOffset
    }

acquireFd path omode fmode flags =
    Acquire.mkAcquire
        (PIO.openFd path omode fmode flags)
        PIO.closeFd

open :: FilePath -> FileOffset -> Acquire FileArena
open path off = do
    fd <- acquireFd path PIO.ReadWrite (Just 0o700) PIO.defaultFileFlags
    liftIO $ fromFd fd off

fromFd :: Fd -> FileOffset -> IO FileArena
fromFd fd off = do
    Unix.ftruncateExn fd off
    nextAlloc <- newTVarIO off
    pure FileArena { fd, nextAlloc }

alloc :: ByteCount -> FileArena -> STM FileOffset
alloc count FileArena{nextAlloc} = do
    offset <- readTVar nextAlloc
    writeTVar nextAlloc $! offset + fromIntegral count
    pure offset

writeBS :: BS.ByteString -> FileArena -> IO FileOffset
writeBS bs = writeLBS (LBS.fromStrict bs)

writeLBS :: LBS.ByteString -> FileArena -> IO FileOffset
writeLBS lbs arena@FileArena{fd} = do
    offset <- atomically $ alloc (fromIntegral $ LBS.length lbs) arena
    pwritev fd (LBS.toChunks lbs) offset
    pure offset

-- TODO: replace this with a wrapper for the pwritev() syscall, maybe pushing one
-- upstream.
pwritev :: Fd -> [BS.ByteString] -> FileOffset -> IO ()
pwritev fd chunks offset = case chunks of
    [] -> pure ()
    (c:cs) -> do
        -- FIXME: somewhere we need to deal with short writes
        Unix.pwrite fd c offset
        pwritev fd cs (offset + fromIntegral (BS.length c))



----

readMsg :: Addr a -> FileArena -> IO (Capnp.Message 'Capnp.Const)
readMsg Addr{offset, length, flatMessage} FileArena{fd} = do
    bytes <- Unix.preadExn fd (fromIntegral length) (fromIntegral offset)
    if flatMessage
        then pure $ M.singleSegment $ Capnp.fromByteString bytes
        else Capnp.bsToMsg bytes

writeMsg :: Capnp.Message 'Capnp.Const -> FileArena -> IO (Addr a)
writeMsg msg arena = do
    nsegs <- Capnp.numSegs msg
    if nsegs == 1
        then (do
            seg <- M.getSegment msg 0
            writeSingleSegment seg arena)
        else
            writeMultiSegment msg arena

writeSingleSegment :: Capnp.Segment 'Capnp.Const -> FileArena -> IO (Addr a)
writeSingleSegment seg arena = do
    let bs = Capnp.toByteString seg
    offset <- writeBS bs arena
    pure Addr
        { offset = fromIntegral offset
        , length = fromIntegral $ BS.length bs
        , flatMessage = True
        }

writeMultiSegment :: Capnp.Message 'Capnp.Const -> FileArena -> IO (Addr a)
writeMultiSegment msg arena = do
    let lbs = Capnp.msgToLBS msg
    offset <- writeLBS lbs arena
    pure Addr
        { offset = fromIntegral offset
        , length = fromIntegral $ LBS.length lbs
        , flatMessage = False
        }
