{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile.DiskTrie
    ( lookup
    ) where

import qualified BlobStore.BigFile.FileArena as FA
import           BlobStore.BigFile.TrieError
import           BlobStore.BigFile.TrieKey   (Key)
import qualified BlobStore.BigFile.TrieKey   as Key
import qualified Capnp
import           Capnp.Gen.DiskBigfile.Pure
import           Control.Exception.Safe      (throwIO)
import qualified Data.Vector                 as V
import           Zhp

lookup :: Capnp.ReadParam a => Key a -> TrieMap a -> FA.FileArena (TrieMap'Branch a) -> IO (Maybe a)
lookup key trie fa = go key trie where
    go key = \case
        TrieMap'empty -> pure Nothing
        TrieMap'leaf TrieMap'leaf'{value, keySuffix}
            | keySuffix == Key.bytes key -> pure $ Just value
            | otherwise -> pure $ Nothing
        TrieMap'branch branchAddr -> do
            TrieMap'Branch kids <- FA.readValue branchAddr fa
            let (k, ks) = Key.uncons key
            go ks (kids V.! k)
        TrieMap'unknown' n ->
            throwIO $ ErrUnknownDiskTrieVariant n
