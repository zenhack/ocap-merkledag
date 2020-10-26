{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Client
    ( storeFile
    ) where

import Zhp

import System.FilePath (takeFileName)
import System.IO       (withFile)

import           Capnp     (def)
import           Capnp.Rpc ((?))
import qualified Capnp.Rpc as Rpc

import qualified Capnp.Gen.Files.Pure    as Files
import qualified Capnp.Gen.Protocol.Pure as P
import qualified Capnp.Gen.Util.Pure     as Util

import qualified Data.ByteString as BS

storeFile :: FilePath -> P.Store -> IO (P.Hash, P.Ref)
storeFile path store = do
    (contentRef, size) <- withFile path ReadMode (storeHandleBlob store)
    let file = Files.File
            { Files.name = fromString $ takeFileName path
            -- TODO: stat, fill in metadata:
            , Files.ctime = 0
            , Files.mtime = 0
            , Files.permissions = 0o644

            , Files.union' = Files.File'file Files.File'file'
                { Files.contents = contentRef
                , Files.size = size
                }
            }
    let ptr = error "TODO: turn file into an AnyPointer"
    P.Store'put'results{hash, ref} <-
        Rpc.wait =<< P.store'put store ? P.Store'put'params { value = ptr }
    pure (hash, ref)


storeHandleBlob :: P.Store -> Handle -> IO (P.Ref, Word64)
storeHandleBlob store h = do
    P.Store'putBytesStreaming'results{stream, ref} <-
        Rpc.wait =<< P.store'putBytesStreaming store ? def
    size <- streamHandle h stream
    pure (ref, size)

streamHandle :: Handle -> Util.ByteStream -> IO Word64
streamHandle h stream = go 0
  where
    go !size = do
        bytes <- BS.hGet h blockSize
        if bytes == BS.empty
            then (do
                _ <- Rpc.wait =<< Util.byteStream'done stream ? def
                pure size)
            else (do
                Rpc.wait =<< Util.byteStream'write stream ? Util.ByteStream'write'params { data_ = bytes }
                go $ size + fromIntegral (BS.length bytes))

-- | Relatively arbitrary size of a chunk to upload at a time.
-- currently 64KiB.
blockSize :: Int
blockSize = 64 * 1024
