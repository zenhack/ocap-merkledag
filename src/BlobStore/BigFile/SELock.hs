module BlobStore.BigFile.SELock
    ( SELock
    , new
    , acquireShared
    , acquireExclusive
    ) where

import Control.Concurrent.STM
import Lifetimes
import Zhp

data SELock = SELock
    { excludeState :: TVar ExcludeState
    , shared       :: TVar Int
    }

data ExcludeState
    = None
    | Pending
    | Active

new :: STM SELock
new = SELock
    <$> newTVar None
    <*> newTVar 0

sharedLock :: SELock -> STM ()
sharedLock l = do
    state <- readTVar (excludeState l)
    case state of
        None -> modifyTVar' (shared l) (+1)
        _    -> retry

sharedUnlock:: SELock -> STM ()
sharedUnlock l =
    modifyTVar' (shared l) (\x -> x - 1)

startWriteLock :: SELock -> STM ()
startWriteLock l = do
    state <- readTVar (excludeState l)
    case state of
        None -> writeTVar (excludeState l) Pending
        _    -> retry

finishExclusiveLock :: SELock -> STM ()
finishExclusiveLock l = do
    s <- readTVar (shared l)
    when (s > 0) retry
    writeTVar (excludeState l) Active

exclusiveUnlock :: SELock -> STM ()
exclusiveUnlock l =
    writeTVar (excludeState l) None


acquireExclusive :: SELock -> Acquire ()
acquireExclusive l = do
    mkAcquire
        (atomically $ startWriteLock l)
        (const $ atomically $ exclusiveUnlock l)
    liftIO $ atomically $ finishExclusiveLock l

acquireShared :: SELock -> Acquire ()
acquireShared l =
    mkAcquire
        (atomically $ sharedLock l)
        (const $ atomically $ sharedUnlock l)
