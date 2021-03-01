{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | This module defines the storage-independent parts of a blob store.
module BlobStore
    ( KnownHash(..)
    , IllegalHash(..)
    , RawBlobStore(..)
    , BlobStore
    , computeHash
    , encodeHash
    , decodeHash
    , fromRaw
    , getBlob
    , putBlob
    , hasBlob
    , syncFile
    ) where

import Zhp

import qualified Capnp.Gen.Disk.Pure     as Disk
import           Capnp.Gen.Protocol.Pure
import           Capnp.Gen.Storage.Pure

import Capnp.Classes (toStruct)
import Capnp.Message (singleSegment)

import qualified Capnp
import qualified Capnp.Untyped.Pure     as U
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
-- * The digest is known to be the correct length for the algorithm.
data KnownHash = Sha256 (Digest SHA256)
    deriving(Show, Eq, Ord)

-- | An exception indicating that a 'Hash' is not a valid 'KnownHash'.
data IllegalHash
    = UnsupportedAlgo HashAlgo -- ^ Unsupported hash algorithm.
    | WrongDigestLen -- ^ Digest length is wrong for the specified algorithm.
    deriving(Show)
instance Exception IllegalHash

data CorruptedBlob = CorruptedBlob
    { expectedHash :: KnownHash
    , actualHash   :: KnownHash
    } deriving(Show)
instance Exception CorruptedBlob

-- | A raw store for bytes. Supports setting and getting blob contents by hash,
-- but does not compute or validate the hashes itself. You will want to wrap
-- this in a 'BlobStore' via 'fromRaw' before using it.
data RawBlobStore m = RawBlobStore
    { getBlobRaw :: KnownHash -> m BS.ByteString
    , putBlobRaw :: KnownHash -> BS.ByteString -> m ()
    , hasBlobRaw :: KnownHash -> m Bool
    , syncRaw    :: m Disk.StoreInfo
    }

-- | A blob store which validates its contents.
newtype BlobStore m = BlobStore { rawStore :: RawBlobStore m }

-- | Wrap a 'RawBlobStore' into a 'BlobStore'.
fromRaw :: RawBlobStore m -> BlobStore m
fromRaw = BlobStore

-- | Fetch a blob from the store.
getBlob :: MonadThrow m => BlobStore m -> Hash -> m (StoredBlob (Maybe U.Ptr))
getBlob bs hash = do
    wantHash <- require $ decodeHash hash
    bytes <- getBlobRaw (rawStore bs) wantHash
    let gotHash = computeHash bytes
    when (wantHash /= gotHash) $
        throwM CorruptedBlob
            { expectedHash = wantHash
            , actualHash = gotHash
            }
    let msg = singleSegment $ Capnp.fromByteString bytes
    Capnp.msgToValue msg

-- | Add a blob to the store, canonicalizing it and returning the resulting hash.
putBlob :: MonadThrow m => BlobStore m -> StoredBlob (Maybe U.Ptr) -> m Hash
putBlob bs blob = do
    seg :: Capnp.Segment 'Capnp.Const <- Capnp.createPure maxBound $ do
        msg <- Capnp.newMessage Nothing
        rawBlob <- Capnp.cerialize msg blob
        (_, seg) <- Capnp.canonicalize (toStruct rawBlob)
        pure seg
    let bytes = Capnp.toByteString seg
    let digest = computeHash bytes
    putBlobRaw (rawStore bs) digest bytes
    pure $ encodeHash digest

hasBlob :: MonadThrow m => BlobStore m -> Hash -> m Bool
hasBlob store hash = do
    wantHash <- require $ decodeHash hash
    hasBlobRaw (rawStore store) wantHash

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
computeHash :: BS.ByteString -> KnownHash
computeHash bytes = Sha256 $ CH.hash bytes

syncFile :: BlobStore IO -> FilePath -> IO ()
syncFile bs path = do
    info <- syncRaw (rawStore bs)
    bytes <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS info
    atomicWriteFile (path <> "/StoreInfo") bytes

atomicWriteFile :: FilePath -> LBS.ByteString -> IO ()
atomicWriteFile path bytes = do
    LBS.writeFile (path <> ".new") bytes
    -- TODO/FIXME: sync, rename
