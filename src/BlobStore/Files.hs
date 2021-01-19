{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module implements a 'RawBlobStore' on top of a local filesystem.
module BlobStore.Files
    ( open
    ) where

-- The format of the store is a directory tree with the following structure:
--
-- * `tmp/` - empty at rest but used for temporary scratch space.
-- * `blobs/` - stores the actual on-disk blobs
--   * `sha256/` - blobs stored with a sha256 digest (currently the only
--     supported hash). Within this there are up to 256 directores named
--     `00` to `ff`, where each corresponds to the first byte of the hash.
--     each of those has another up to 256 directories with the same name
--     corresponding to the second byte of the hash, and each of those
--     contains regular files with the actual contents of the blobs,
--     named with the remainder of the hash in hexidecimal. The directories
--     are created on demand, the first time a blob would go in them.

import Zhp

import BlobStore

import Capnp.Gen.Disk.Pure

import           Control.Exception.Safe
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import           System.Directory
    (createDirectoryIfMissing, doesPathExist)
import           System.FilePath        (takeDirectory)
import           System.IO.Error        (isDoesNotExistError)

import Crypto.Hash (Digest, SHA256)

newtype FilesBlobStore = FilesBlobStore { storePath :: FilePath }

-- | Open a raw blob store stored at the given path, initializing it if
-- necessary.
open :: FilePath -> IO (RawBlobStore IO)
open storePath = do
    let fbs = FilesBlobStore{storePath}
    initStore fbs
    pure RawBlobStore
        { getBlobRaw = filesGetRaw fbs
        , putBlobRaw = filesPutRaw fbs
        , hasBlobRaw = filesHasRaw fbs
        , syncRaw = pure $ StoreInfo'loose LooseStoreInfo
        }

-- | Initialize the store (creating the directory structure
-- described above).
initStore :: FilesBlobStore -> IO ()
initStore FilesBlobStore{storePath} = do
    mkdirP $ storePath <> "/tmp"
    mkdirP $ storePath <> "/blobs/sha256"

mkdirP :: FilePath -> IO ()
mkdirP = createDirectoryIfMissing True

-- | Get a blob from the store.
filesGetRaw :: FilesBlobStore -> KnownHash -> IO BS.ByteString
filesGetRaw fbs hash =
    BS.readFile (hashPath fbs hash)

-- | Put a blob into the store.
filesPutRaw :: FilesBlobStore -> KnownHash -> BS.ByteString -> IO ()
filesPutRaw fbs hash bytes = do
    -- FIXME: do this atomically:
    let path = hashPath fbs hash
        writeFileBytes = BS.writeFile (hashPath fbs hash) bytes
    res <- try writeFileBytes
    case res of
        Right () -> pure ()
        Left e
            | isDoesNotExistError e -> do
                -- lazily create parent directories as needed:
                mkdirP (takeDirectory path)
                writeFileBytes
            | otherwise ->
                throwIO e


filesHasRaw :: FilesBlobStore -> KnownHash -> IO Bool
filesHasRaw fbs hash = doesPathExist (hashPath fbs hash)

-- | Compute the path in which to store a blob with the given digest.
hashPath :: FilesBlobStore -> KnownHash -> FilePath
hashPath fbs (Sha256 d) = sha256Path fbs d

-- | Compute the path in which to store a blob with the given sha256 digest.
sha256Path :: FilesBlobStore -> Digest SHA256 -> FilePath
sha256Path FilesBlobStore{storePath} digest =
    printf "%s/blobs/sha256/%02x/%02x/%s"
        storePath
        (BA.index digest 0)
        (BA.index digest 1)
        (digest
            & BA.unpack
            & drop 2
            & map (printf "%02x")
            & mconcat
            :: String
        )
