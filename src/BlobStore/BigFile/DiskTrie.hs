module BlobStore.BigFile.DiskTrie
    ( lookup
    ) where

import qualified BlobStore.BigFile.FileArena as FA
import           BlobStore.BigFile.TrieKey   (Key)
import qualified BlobStore.BigFile.TrieKey   as Key
import           Capnp.Gen.DiskBigfile

lookup :: Key a -> TriePtr a -> FA.FileArena (TrieBranch a) -> IO (Maybe (Addr a))
lookup key trie fa = go key trie where
    go key = \case
        TriePtr'empty -> pure Nothing
        TriePtr'leaf TriePtr'leaf'{addr, keySuffix}
            | keySuffix == Key.bytes key -> pure $ Just addr
            | otherwise -> pure $ Nothing
        TriePtr'branch (TriePtr'branch' branchAddr) -> do
            TrieBranch kids <- FA.readValue fa branchAddr
            let (k, ks) = Key.uncons key
            go ks (kids V.! k)
