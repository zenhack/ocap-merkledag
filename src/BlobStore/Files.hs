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

open :: FilePath -> IO (BlobStore IO)
open storePath = do
    let fbs = FilesBlobStore{storePath}
    initStore fbs
    pure BlobStore
        { getBlob = filesGetBlob fbs
        , putBlob = filesPutBlob fbs
        }

initStore :: FilesBlobStore -> IO ()
initStore FilesBlobStore{storePath} = do
    let mkdirP = createDirectoryIfMissing True
    mkdirP $ storePath <> "/tmp"
    for_ [0..0xff] $ \(a :: Word8) ->
        for_ [0..0xff] $ \(b :: Word8) ->
            mkdirP $ printf "%s/sha256/%02x/%02x" storePath a b

data KnownHash = Sha256 (Digest SHA256)

data IllegalHash
    = UnsupportedAlgo HashAlgo
    | WrongDigestLen
    deriving(Show)
instance Exception IllegalHash

require :: (MonadThrow m, Exception e) => Either e a -> m a
require (Left e)  = throw e
require (Right v) = pure v

decodeHash :: Hash -> Either IllegalHash KnownHash
decodeHash Hash{digest, algo} = case algo of
    HashAlgo'unknown' _ -> Left $ UnsupportedAlgo algo
    HashAlgo'sha256 -> case digestFromByteString digest of
        Just d  -> Right $ Sha256 d
        Nothing -> Left WrongDigestLen

encodeHash :: KnownHash -> Hash
encodeHash (Sha256 digest) = Hash
    { algo = HashAlgo'sha256
    , digest = BA.convert digest
    }

computeHash :: LBS.ByteString -> KnownHash
computeHash bytes = Sha256 $ CH.hashlazy bytes

filesGetBlob :: FilesBlobStore -> Hash -> IO StoredBlob
filesGetBlob fbs hash = do
    h <- require $ decodeHash hash
    let path = hashPath fbs h
    BS.readFile path >>= Capnp.bsToValue

filesPutBlob :: FilesBlobStore -> StoredBlob -> IO Hash
filesPutBlob fbs blob = do
    -- TODO: canonicalize (needs support in haskell-capnp)
    bytes <- Capnp.evalLimitT maxBound $ Capnp.valueToLBS blob
    let digest = computeHash bytes
        path = hashPath fbs digest
    -- FIXME: do this atomically:
    LBS.writeFile path bytes
    pure $ encodeHash digest

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
