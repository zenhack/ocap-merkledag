{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile.MemTrie
    ( MemTrie
    , Key
    , makeKey
    , empty
    , focus
    , lookup
    , insert
    , remove
    , mergeToDisk
    ) where

import qualified BlobStore.BigFile.FileArena as FA
import qualified Capnp
import           Capnp.Gen.DiskBigfile.Pure
import           Control.Concurrent.STM
import           Control.Monad.ST            (RealWorld)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Vector                 as V
import           Zhp                         hiding (empty)

newtype Key a = Key BS.ByteString
    deriving(Eq)

expectedLength :: Int
expectedLength = 256 `div` 8

uncons :: Key a -> (Int, Key a)
uncons (Key bytes) =
    case BS.uncons bytes of
        Just (b, bs) -> (fromIntegral b, Key bs)
        Nothing      -> error "BUG: uncons on empty key."

makeKey :: BS.ByteString -> Maybe (Key a)
makeKey bytes
    | BS.length bytes == expectedLength = Just (Key bytes)
    | otherwise = Nothing

data MemTrie a
    = Leaf (Key a) a
    | Branch (V.Vector (MemTrie a))
    | Empty

empty :: MemTrie a
empty = Empty

focus :: Key a -> MemTrie a -> (Maybe a, Maybe a -> MemTrie a)
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
            Nothing -> Leaf key' v'
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
    , \v -> Branch $ kids V.// [(fromIntegral b, mkKid v)]
    )

lookup :: Key a -> MemTrie a -> Maybe a
lookup key trie =
    fst (focus key trie)

insert :: Key a -> a -> MemTrie a -> MemTrie a
insert key value trie =
    snd (focus key trie) (Just value)

remove :: Key a -> MemTrie a -> MemTrie a
remove key trie =
    snd (focus key trie) Nothing

mergeToDisk ::
    ( Capnp.ReadParam a
    , Capnp.WriteParam RealWorld a
    ) => MemTrie (Addr a) -> TriePtr a -> FA.FileArena (TrieBranch a) -> IO (TriePtr a)
mergeToDisk mem disk arena =
    fst <$> go mem disk
  where
    go Empty disk =
        pure (disk, False)
    go mem TriePtr'empty = do
        ret <- writeTo mem arena
        pure (ret, ret /= TriePtr'empty)
    go mem@(Leaf (Key k) v) (TriePtr'leaf TriePtr'leaf'{addr, keySuffix})
        | k == keySuffix = do
            ret <- writeTo mem arena
            pure (ret, True)
        | otherwise = do
            ret <- writeTo (insert (Key keySuffix) addr mem) arena
            pure (ret, True)
    go (Branch memKids) disk@(TriePtr'branch (TriePtr'branch' branchAddr)) = do
        TrieBranch diskKids <- FA.readValue branchAddr arena
        results <- sequence $ V.zipWith go memKids diskKids
        let changed = any snd results
        if changed
            then (do
                let diskKids' = V.map fst results
                lbs <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS $ TrieBranch diskKids'
                off <- FA.writeLBS lbs arena
                pure
                    ( TriePtr'branch $ TriePtr'branch' Addr
                        { offset = fromIntegral off
                        , length = fromIntegral $ LBS.length lbs
                        , flatMessage = False
                        }
                    , True
                    )
                )
            else
                pure (disk, False)

writeTo :: (Capnp.ReadParam a, Capnp.WriteParam RealWorld a)
    => MemTrie (Addr a) -> FA.FileArena (TrieBranch a) -> IO (TriePtr a)
writeTo Empty _ = pure TriePtr'empty
writeTo (Leaf (Key keySuffix) addr) _ =
    pure $ TriePtr'leaf TriePtr'leaf'{keySuffix, addr}
writeTo (Branch kids) arena = do
    kids' <- traverse (`writeTo` arena) kids
    lbs <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS (TrieBranch kids')
    off <- FA.writeLBS lbs arena
    pure $ TriePtr'branch $ TriePtr'branch' Addr
        { offset = fromIntegral off
        , length = fromIntegral $ LBS.length lbs
        , flatMessage = False
        }
