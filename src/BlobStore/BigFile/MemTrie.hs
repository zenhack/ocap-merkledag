{-# LANGUAGE LambdaCase #-}
module BlobStore.BigFile.MemTrie
    ( MemTrie
    , Key
    , makeKey
    , focus
    , insert
    , update
    , remove
    ) where

import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import qualified Data.Vector            as V
import           Zhp

import qualified Capnp.Gen.DiskBigfile.Pure as DBF

newtype Key a = Key BS.ByteString
    deriving(Eq)

expectedLength :: Int
expectedLength = 256 `div` 8

uncons :: Key -> (Int, Key)
uncons (Key bytes) =
    case BS.uncons bytes of
        Just (b, bs) -> Just (fromIntegral b, Key bs)
        Nothing      -> error "BUG: uncons on empty key."

makeKey :: BS.ByteString -> Maybe Key
makeKey bytes
    | BS.length bytes == expectedLength = Just (Key bytes)
    | otherwise = Nothing

data MemTrie a
    = Leaf BS.ByteString a
    | Branch (V.Vector (MemTrie a))
    | Empty

focus :: BS.ByteString -> MemTrie a -> (Maybe a, Maybe a -> MemTrie a)
focus key Empty =
    ( Nothing
    , \case
        Just x  -> Leaf key x
        Nothing -> Empty
    )
focus key (Leaf key' v')
    | key == key' =
        ( Just v'
        , \case
            Just x  -> Leaf key' x
            Nothing -> Empty
        )
    | otherwise =
        ( Nothing
        , \case
            Nothing -> Leaf key' value
            Just v ->
                let (k, ks) = uncons key
                    (k', ks') = uncons key'
                in
                Branch $ V.generate 256 $ \i ->
                    if i == k then
                        Leaf ks v
                    else if i == k' then
                        Leaf ks' v'
                    else
                        Empty
        )
focus key (Branch kids) =
    let (b, bs) = uncons key
        (value, mkKid) = focus bs (kids V.! b)
    in
    ( value
    , \v -> kids V.// [(fromIntegral b, mkKid v)]
    )

lookup :: Key -> MemTrie a -> Maybe a
lookup key trie =
    fst (focus key trie)

insert :: Key -> a -> MemTrie a -> MemTrie a
insert key value trie =
    snd (focus key trie) (Just value)

remove :: Key -> a -> MemTrie a -> MemTrie a
remove key trie =
    snd (focus key trie) Nothing

flush :: MemTrie (Addr a) -> DBF.TriePtr a -> FA.FileArena (DBF.TriePtr a) -> IO (DBF.TriePtr a)
flush mem disk arena =
    fst <$> go mem disk
  where
    go Empty disk = pure (disk, False)
    go mem DBF.TriePtr'empty ->
        writeTo mem arena
    go mem
        -- Optimization: for an empty MemTrie, we don't need to load the disk trie at all.
        pure (disk, False)
    go mem diskAddr = do
        disk <- FA.readValue diskAddr arena
        case (mem, disk) of
            (Empty, _) -> pure (diskAddr, False)
            (_, DBF.TriePtr'empty) -> do
                -- TODO(perf): specialize (empty, empty)
                writeTo mem arena
    Empty disk = pure (disk, False)

{-
flush :: TVar (MemTrie (Addr a)) -> Addr (DBF.TriePtr a) -> FA.FileArena (DBF.TriePtr a) -> IO (Addr (DBF.TriePtr a))
flush memVar diskAddr fa = do
    mem <- readTVar memVar
    disk <- FA.readValue diskAddr fa
    case (mem, disk) of
        (Empty, _) -> pure disk
        (Leaf keySuffix addr, TriePtr'empty) -> do
            msg <- Capnp.createPure maxBound $ do
                Capnp.valueToMsg TriePtr'leaf{leaf, keySuffix}
            FA.writeMsg msg fa
        (Branch kids, TriePtr'empty) -> do
            error "TODO"

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
-}
