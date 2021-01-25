{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
module BlobStore.BigFile.BackedTrie
    ( BackedTrie
    , make
    , flush
    , lookup
    ) where

import qualified BlobStore.BigFile.DiskTrie  as DiskTrie
import qualified BlobStore.BigFile.FileArena as FA
import qualified BlobStore.BigFile.MemTrie   as MemTrie
import           BlobStore.BigFile.TrieKey
import           Capnp                       (ReadParam, WriteParam)
import           Capnp.Gen.DiskBigfile.Pure
import           Control.Concurrent.STM
import           Control.Monad.ST            (RealWorld)
import           Zhp

data BackedTrie a = BackedTrie
    { memW  :: TVar (MemTrie.MemTrie a)
    , memR  :: TVar (Maybe (MemTrie.MemTrie a))
    , disk  :: TVar (TrieMap a)
    , arena :: FA.FileArena (TrieMap'Branch a)
    }

make :: TrieMap a -> FA.FileArena (TrieMap'Branch a) -> STM (BackedTrie a)
make d arena = do
    memW <- newTVar MemTrie.empty
    memR <- newTVar Nothing
    disk <- newTVar d
    pure BackedTrie {memW, memR, disk, arena}

flush
    :: (ReadParam a, WriteParam RealWorld a)
    => BackedTrie a
    -> ([Addr (TrieMap'Branch a)] -> STM ())
    -> STM (IO ())
flush t@BackedTrie{arena} free = do
    oldR <- readTVar (memR t)
    when (not (isNothing oldR)) retry
    oldW <- readTVar (memW t)
    writeTVar (memR t) (Just oldW)
    writeTVar (memW t) MemTrie.empty
    diskT <- readTVar (disk t)
    pure $ do
        (newDisk, toFree) <- MemTrie.mergeToDisk oldW diskT arena
        atomically $ do
            writeTVar (disk t) newDisk
            writeTVar (memR t) Nothing
            free toFree

lookup :: ReadParam a => Key a -> BackedTrie a -> STM (IO (Maybe a))
lookup k t@BackedTrie{arena} = do
    searchW <- MemTrie.lookup k <$> readTVar (memW t)
    case searchW of
        Just v -> pure $ pure $ Just v
        Nothing ->  do
            searchR <- join . fmap (MemTrie.lookup k) <$> readTVar (memR t)
            case searchR of
                Just v -> pure $ pure $ Just v
                Nothing -> do
                    d <- readTVar (disk t)
                    pure $ DiskTrie.lookup k d arena
