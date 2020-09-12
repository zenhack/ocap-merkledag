{-# LANGUAGE NamedFieldPuns #-}
module BlobStore
    ( KnownHash(..)
    , IllegalHash(..)
    , RawBlobStore(..)
    , BlobStore
    , fromRaw
    , getBlob
    , putBlob
    ) where

import Zhp

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage.Pure

import qualified Capnp
import           Control.Exception.Safe
import           Crypto.Hash            (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash            as CH
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS

data KnownHash = Sha256 (Digest SHA256)

data IllegalHash
    = UnsupportedAlgo HashAlgo
    | WrongDigestLen
    deriving(Show)
instance Exception IllegalHash

data RawBlobStore m = RawBlobStore
    { getBlobRaw :: KnownHash -> m BS.ByteString
    , putBlobRaw :: KnownHash -> LBS.ByteString -> m ()
    }

newtype BlobStore m = BlobStore { rawStore :: RawBlobStore m }

fromRaw :: RawBlobStore m -> BlobStore m
fromRaw = BlobStore

getBlob :: MonadThrow m => BlobStore m -> Hash -> m StoredBlob
getBlob bs hash = do
    h <- require $ decodeHash hash
    getBlobRaw (rawStore bs) h >>= Capnp.bsToValue

putBlob :: MonadThrow m => BlobStore m -> StoredBlob -> m Hash
putBlob bs blob = do
    -- TODO: canonicalize (needs support in haskell-capnp)
    msg <- Capnp.createPure maxBound $ Capnp.valueToMsg blob
    let bytes = Capnp.msgToLBS msg
        digest = computeHash bytes
    putBlobRaw (rawStore bs) digest bytes
    pure $ encodeHash digest

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
