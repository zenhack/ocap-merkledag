{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile.MemTrie
    ( MemTrie
    , empty
    , focus
    , lookup
    , insert
    , remove
    , mergeToDisk
    ) where

import qualified BlobStore.BigFile.FileArena as FA
import           BlobStore.BigFile.TrieError
import           BlobStore.BigFile.TrieKey   (Key(..))
import qualified BlobStore.BigFile.TrieKey   as Key
import qualified Capnp
import           Capnp.Gen.DiskBigfile.Pure
import           Control.Exception.Safe      (throwIO)
import           Control.Monad.ST            (RealWorld)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Vector                 as V
import           Zhp                         hiding (empty)

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
                let (k, ks) = Key.uncons key
                    (k', ks') = Key.uncons key'
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
    let (b, bs) = Key.uncons key
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
    ) => MemTrie a -> TrieMap a -> FA.FileArena (TrieMap'Branch a) -> IO (TrieMap a)
mergeToDisk mem disk arena =
    fst <$> go mem disk
  where
    go _ (TrieMap'unknown' n) =
        throwIO $ ErrUnknownDiskTrieVariant n
    go Empty disk =
        pure (disk, False)
    go mem TrieMap'empty = do
        ret <- writeTo mem arena
        pure
            ( ret
            , case ret of
                TrieMap'empty -> False
                _             -> True
            )
    go mem@(Leaf k _v) (TrieMap'leaf TrieMap'leaf'{value, keySuffix})
        | Key.bytes k == keySuffix = do
            ret <- writeTo mem arena
            pure (ret, True)
        | otherwise = do
            ret <- writeTo (insert (Key keySuffix) value mem) arena
            pure (ret, True)
    go (Leaf _ _) _ = throwIO ErrExpectedLeaf
    go (Branch memKids) disk@(TrieMap'branch branchAddr) = do
        TrieMap'Branch diskKids <- FA.readValue branchAddr arena
        results <- sequence $ V.zipWith go memKids diskKids
        let changed = any snd results
        if changed
            then (do
                let diskKids' = V.map fst results
                lbs <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS $ TrieMap'Branch diskKids'
                off <- FA.writeLBS lbs arena
                pure
                    ( TrieMap'branch Addr
                        { offset = fromIntegral off
                        , length = fromIntegral $ LBS.length lbs
                        , flatMessage = False
                        }
                    , True
                    )
                )
            else
                pure (disk, False)
    go (Branch _) _ = throwIO ErrExpectedBranch

writeTo :: (Capnp.ReadParam a, Capnp.WriteParam RealWorld a)
    => MemTrie a -> FA.FileArena (TrieMap'Branch a) -> IO (TrieMap a)
writeTo Empty _ = pure TrieMap'empty
writeTo (Leaf k value) _ =
    let keySuffix = Key.bytes k in
    pure $ TrieMap'leaf TrieMap'leaf'{keySuffix, value}
writeTo (Branch kids) arena = do
    kids' <- traverse (`writeTo` arena) kids
    lbs <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS (TrieMap'Branch kids')
    off <- FA.writeLBS lbs arena
    pure $ TrieMap'branch Addr
        { offset = fromIntegral off
        , length = fromIntegral $ LBS.length lbs
        , flatMessage = False
        }
