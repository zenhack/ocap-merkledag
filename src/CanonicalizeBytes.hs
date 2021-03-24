{-# LANGUAGE ScopedTypeVariables #-}
module CanonicalizeBytes
    ( canonicalizeBytesBlob
    ) where

import           Capnp.Bits
import qualified Capnp.Pointer           as P
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS
import           Zhp

canonicalizeBytesBlob :: BS.ByteString -> LBS.ByteString
canonicalizeBytesBlob bytes =
    let nBytes :: ByteCount = fromIntegral (BS.length bytes)
        nWords = bytesToWordsCeil nBytes
        nPadBytes = wordsToBytes nWords - nBytes
        segmentTotalWords = nWords + 5
    in
    BB.toLazyByteString $ mconcat
        [ BB.word32LE 0 -- 1 segment
        , BB.word32LE (fromIntegral segmentTotalWords) -- segment length
        , ptr $ Just $ P.StructPtr 0 0 2 -- Root ptr
        , ptr $ Just $ P.StructPtr 1 0 1 -- StoredBlob.data
        , ptr $ Just $ P.ListPtr (fromIntegral $ nWords + 1) $ P.EltComposite 0 -- StoredBlob.ptrs
        , ptr $ Just $ P.ListPtr 0 $ P.EltNormal P.Sz8 $ fromIntegral nBytes -- BlobTree.leaf
        , BB.byteString bytes
        , mconcat $ take (fromIntegral nPadBytes) $ repeat (BB.word8 0)
        , BB.word64LE 0 -- tag word for the cap list; empty.
        ]
  where
    ptr = BB.word64LE . P.serializePtr
