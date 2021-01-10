module BlobStore.BigFile.MemTrie
    ( MemTrie
    ) where

import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import qualified Data.Vector            as V
import           Zhp

data MemTrie a
    = Leaf BS.ByteString a
    | Branch (V.Vector (TVar (MemTrie a)))
    | Empty

lookup :: BS.ByteString -> MemTrie a -> STM (Maybe a)
lookup seek (Leaf k v)
    | seek == k = pure $ Just v
    | otherwise = pure Nothing
lookup seek (Branch kids) = do
    let (b, bs) = uncons seek
    kid <- readTVar $ kids V.! fromIntegral b
    lookup bs kid
lookup _ Empty = pure Nothing

uncons k = case BS.uncons k of
    Just v  -> v
    Nothing -> error "Key is the wrong length" -- TODO: find a way to rule this out

insert :: BS.ByteString -> a -> TVar (MemTrie a) -> STM ()
insert key value var = do
    trie <- readTVar var
    case trie of
        Empty ->
            writeTVar var $! Leaf key value

        Leaf key' _ | key == key' ->
            writeTVar var $! Leaf key value

        Leaf key' value' -> do
            kids <- V.generateM 256 $ \_ -> newTVar Empty
            let (b, bs) = uncons key'
            writeTVar (kids V.! fromIntegral b) $ Leaf bs value'
            writeTVar var (Branch kids)
            insert key value var

        Branch kids ->
            let (b, bs) = uncons key in
            insert bs value (kids V.! fromIntegral b)
