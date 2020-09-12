{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines the storage-independent parts of a blob store.
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

-- | A hash we understand. This is morally the same as 'Hash' from the
-- schema, except:
--
-- * It does not have an unknown variant
-- * The diges is known to be the correct length for the algorithm.
data KnownHash = Sha256 (Digest SHA256)

-- | An exception indicating that a 'Hash' is not a valid 'KnownHash'.
data IllegalHash
    = UnsupportedAlgo HashAlgo -- ^ Unsupported hash algorithm.
    | WrongDigestLen -- ^ Digest length is wrong for the specified algorithm.
    deriving(Show)
instance Exception IllegalHash

-- | A raw store for bytes. Supports setting and getting blob contents by hash,
-- but does not compute or validate the hashes itself. You will want to wrap
-- this in a 'BlobStore' via 'fromRaw' before using it.
data RawBlobStore m = RawBlobStore
    { getBlobRaw :: KnownHash -> m BS.ByteString
    , putBlobRaw :: KnownHash -> LBS.ByteString -> m ()
    }

-- | A blob store which validates its contents.
newtype BlobStore m = BlobStore { rawStore :: RawBlobStore m }

-- | Wrap a 'RawBlobStore' into a 'BlobStore'.
fromRaw :: RawBlobStore m -> BlobStore m
fromRaw = BlobStore

-- | Fetch a blob from the store.
getBlob :: MonadThrow m => BlobStore m -> Hash -> m StoredBlob
getBlob bs hash = do
    h <- require $ decodeHash hash
    -- TODO: add an integrity check.
    getBlobRaw (rawStore bs) h >>= Capnp.bsToValue

-- | Add a blob to the store, canonicalizing it and returning the resulting hash.
putBlob :: MonadThrow m => BlobStore m -> StoredBlob -> m Hash
putBlob bs blob = do
    -- TODO: canonicalize (needs support in haskell-capnp)
    msg <- Capnp.createPure maxBound $ Capnp.valueToMsg blob
    let bytes = Capnp.msgToLBS msg
        digest = computeHash bytes
    putBlobRaw (rawStore bs) digest bytes
    pure $ encodeHash digest

-- | Misc. helper function: unwrap an 'Either' and throw an exception on 'Left'.
require :: (MonadThrow m, Exception e) => Either e a -> m a
require (Left e)  = throw e
require (Right v) = pure v

-- | Decode a protocol hash into a guaranteed valid one.
decodeHash :: Hash -> Either IllegalHash KnownHash
decodeHash Hash{digest, algo} = case algo of
    HashAlgo'unknown' _ -> Left $ UnsupportedAlgo algo
    HashAlgo'sha256 -> case digestFromByteString digest of
        Just d  -> Right $ Sha256 d
        Nothing -> Left WrongDigestLen

-- | Encode a hash for use in the protocol.
encodeHash :: KnownHash -> Hash
encodeHash (Sha256 digest) = Hash
    { algo = HashAlgo'sha256
    , digest = BA.convert digest
    }

-- | Compute the hash of a lazy bytestring.
computeHash :: LBS.ByteString -> KnownHash
computeHash bytes = Sha256 $ CH.hashlazy bytes
