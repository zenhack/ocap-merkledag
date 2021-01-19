module BlobStore.BigFile.TrieError
    ( DiskInconsistencyError(..)
    ) where

import Control.Exception.Safe (Exception)
import Zhp

data DiskInconsistencyError
    = ErrUnknownDiskTrieVariant Word16
    | ErrExpectedBranch
    | ErrExpectedLeaf
    deriving(Show)
instance Exception DiskInconsistencyError
