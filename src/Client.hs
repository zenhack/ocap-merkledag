{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Client
    ( storeFile
    ) where

import Zhp

import System.FilePath (takeFileName, (</>))
import System.IO       (withFile)

import Foreign.C.Types (CTime (..))

import qualified System.Posix.Files as Posix

import           Capnp     (def)
import qualified Capnp
import           Capnp.Rpc ((?))
import qualified Capnp.Rpc as Rpc

import qualified Capnp.Gen.Files.Pure    as Files
import qualified Capnp.Gen.Protocol.Pure as P
import qualified Capnp.Gen.Util.Pure     as Util

import qualified Data.ByteString as BS

data FileStoreError
    = ErrUnsupportedFileType
    deriving(Show)

storeFile :: FilePath -> P.Store -> IO (Either FileStoreError Files.File)
storeFile path store = do
    status <- Posix.getSymbolicLinkStatus path
    let CTime modTime = Posix.modificationTime status
    storeFileUnion status path store >>= \case
        Left err -> pure $ Left err
        Right union' -> pure $ Right Files.File
            { Files.name = fromString $ takeFileName path
            , Files.modTime = modTime
            , Files.permissions = fromIntegral (Posix.fileMode status) .&. 0o777
            , Files.union' = union'
            }

storePtr :: (Capnp.Cerialize a, Capnp.ToPtr RealWorld (Capnp.Cerial a)) => a -> IO (P.Ref, P.Hash)
storePtr value = do
    let ptr = makePtr value
    P.Store'put'results{hash, ref} <-
        Rpc.wait =<< P.store'put store ? P.Store'put'params { value = ptr }
    pure (hash, ref)

storeFileRef :: FilePath -> P.Store -> IO (P.Hash, P.Ref)
storeFileRef path store = do
    status <- Posix.getSymbolicLinkStatus path
    let CTime modTime = Posix.modificationTime status
    union' <- storeFileUnion status path store
    let file = Files.File
            { Files.name = fromString $ takeFileName path
            , Files.modTime = modTime
            , Files.permissions = fromIntegral (Posix.fileMode status) .&. 0o777
            , Files.union' = union'
            }
    storePtr file store

storeFileUnion :: Posix.FileStatus -> FilePath -> P.Store -> IO (Either FileStoreError Files.File')
storeFileUnion status path store =
    if Posix.isRegularFile status then do
        (contentRef, size) <- withFile path ReadMode (storeHandleBlob store)
        pure $ Files.File'file Files.File'file'
                { Files.contents = contentRef
                , Files.size = size
                }
    else if Posix.isSymbolicLink status then do
        target <- Posix.readSymbolicLink path
        pure $ Files.File'symlink (fromString target)
    else if Posix.isDirectory status then do
        files <- listDirectory path
        refs <- for files $ \file ->
            storeFile (path </> file) store
        let listRef = error "TODO: store the list" refs
        pure $ Files.File'dir listRef
    else
        error "TODO: unknown file type"


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
