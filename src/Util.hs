module Util
    ( acquireAsync
    ) where

import Control.Concurrent.Async
import Lifetimes
import Zhp

acquireAsync :: IO a -> Acquire (Async a)
acquireAsync io = mkAcquire (async io) cancel
