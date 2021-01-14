-- | This module needs reworking.
--
-- The our Trie data types logcally expect fixed-length keys, and this module
-- was intended to proide an abstraction boundary that enforced that keys were
-- the correct size.
--
-- This gets messy though, since we have two trie implementations (on-disk and
-- in-memory), which both need to import this and, as part of their implementation,
-- temporarily use keys of shorter lengths internally. So this module is not
-- really doing its job and the whole situation is a bit awkward.
--
-- We should rethink how all this fits together. Possible directions:
--
-- * Rework the Trie data structures so they can in principle have variable length
--   keys; this would make invalid tries unrepresentable, which is especially nice
--   for the on-disk data structures, beyond the usual benefits. But it's a bit sad
--   that it would require some extra space for a field that is not used.
-- * ???
module BlobStore.BigFile.TrieKey
    ( Key(..)
    , uncons
    , makeKey
    , bytes
    ) where

import qualified Data.ByteString as BS
import           Zhp

newtype Key a = Key BS.ByteString
    deriving(Eq)

expectedLength :: Int
expectedLength = 256 `div` 8

uncons :: Key a -> (Int, Key a)
uncons (Key bytes) =
    case BS.uncons bytes of
        Just (b, bs) -> (fromIntegral b, Key bs)
        Nothing      -> error "uncons on empty key."

makeKey :: BS.ByteString -> Maybe (Key a)
makeKey bytes
    | BS.length bytes == expectedLength = Just (Key bytes)
    | otherwise = Nothing

bytes :: Key a -> BS.ByteString
bytes (Key bs) = bs
