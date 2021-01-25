{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import           Data.Foldable               (toList)
import           Data.Sequence               (Seq, (|>))
import qualified Data.Sequence               as Seq
import           Data.Traversable            (Traversable)
import qualified Data.Vector                 as V
import           Zhp                         hiding (empty)

data MemTrie a
    = Leaf (Key a) a
    | Branch (V.Vector (MemTrie a))
    | Empty
    deriving(Functor, Foldable, Traversable)

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

-- | Merge a MemTrie into its on-disk counterpart. Returns a reference to the
-- new on-disk trie, and a list of addresses in the that are now garbage after
-- the merge.
--
-- TODO/FIXME: The way 'remove' works, I believe this will not actual persist
-- removals.
mergeToDisk ::
    forall a.
    ( Capnp.ReadParam a
    , Capnp.WriteParam RealWorld a
    ) => MemTrie a -> TrieMap a -> FA.FileArena (TrieMap'Branch a) -> IO (TrieMap a, [Addr (TrieMap'Branch a)])
mergeToDisk mem disk arena = do
    (trie, free, _) <- go mem disk
    pure (trie, toList free)
  where
    go :: MemTrie a -> TrieMap a -> IO (TrieMap a, Seq (Addr (TrieMap'Branch a)), Bool)
    go _ (TrieMap'unknown' n) =
        throwIO $ ErrUnknownDiskTrieVariant n
    go Empty disk =
        pure (disk, Seq.empty, False)
    go mem TrieMap'empty = do
        ret <- writeTo mem arena
        pure
            ( ret
            , Seq.empty
            , case ret of
                TrieMap'empty -> False
                _             -> True
            )
    go mem@(Leaf k _v) (TrieMap'leaf TrieMap'leaf'{value, keySuffix})
        | Key.bytes k == keySuffix = do
            ret <- writeTo mem arena
            pure (ret, Seq.empty, True)
        | otherwise = do
            ret <- writeTo (insert (Key keySuffix) value mem) arena
            pure (ret, Seq.empty, True)
    go (Leaf _ _) _ = throwIO ErrExpectedLeaf
    go (Branch memKids) disk@(TrieMap'branch branchAddr) = do
        TrieMap'Branch diskKids <- FA.readValue branchAddr arena
        results <- sequence $ V.zipWith go memKids diskKids
        let changed = or [ c | (_, _, c) <- toList results ]
            free = mconcat [ f | (_, f, _) <- toList results ]
        if changed
            then (do
                let diskKids' = V.map (\(k, _, _) -> k) results
                lbs <- Capnp.evalLimitT Capnp.defaultLimit $ Capnp.valueToLBS $ TrieMap'Branch diskKids'
                off <- FA.writeLBS lbs arena
                pure
                    ( TrieMap'branch Addr
                        { offset = fromIntegral off
                        , length = fromIntegral $ LBS.length lbs
                        , flatMessage = False
                        }
                    , free |> branchAddr
                    , True
                    )
                )
            else
                pure (disk, Seq.empty, False)
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
