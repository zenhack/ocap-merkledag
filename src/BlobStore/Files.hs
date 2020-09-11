{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlobStore.Files
    ( open
    ) where

import Zhp

import BlobStore

import qualified Capnp

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage.Pure

import           Control.Exception.Safe
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           System.Directory       (createDirectoryIfMissing)

import           Crypto.Hash (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash as CH

newtype FilesBlobStore = FilesBlobStore { storePath :: FilePath }

open :: FilePath -> IO (RawBlobStore IO)
open storePath = do
    let fbs = FilesBlobStore{storePath}
    initStore fbs
    pure RawBlobStore
        { getBlobRaw = filesGetRaw fbs
        , putBlobRaw = filesPutRaw fbs
        }

initStore :: FilesBlobStore -> IO ()
initStore FilesBlobStore{storePath} = do
    let mkdirP = createDirectoryIfMissing True
    mkdirP $ storePath <> "/tmp"
    for_ [0..0xff] $ \(a :: Word8) ->
        for_ [0..0xff] $ \(b :: Word8) ->
            mkdirP $ printf "%s/sha256/%02x/%02x" storePath a b

filesGetRaw :: FilesBlobStore -> KnownHash -> IO BS.ByteString
filesGetRaw fbs hash =
    BS.readFile (hashPath fbs hash)

filesPutRaw :: FilesBlobStore -> KnownHash -> LBS.ByteString -> IO ()
filesPutRaw fbs hash bytes =
    -- FIXME: do this atomically:
    LBS.writeFile (hashPath fbs hash) bytes

hashPath :: FilesBlobStore -> KnownHash -> FilePath
hashPath fbs (Sha256 d) = sha256Path fbs d

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
