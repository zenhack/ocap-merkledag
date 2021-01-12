module BlobStore.BigFile.TrieKey
    ( Key
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
