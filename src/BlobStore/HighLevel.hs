{-# LANGUAGE DataKinds #-}
module BlobStore.HighLevel
    ( StoreAPI
    , new

    , LiveRef
    , liveRefHash

    , getRef
    , put

    , getRoot
    , setRoot
    , subscribeRoot

    , checkpoint
    ) where

import           BlobStore
import qualified BlobStore.Raw          as Raw
import qualified Capnp
import           Capnp.Rpc              (Client)
import           Capnp.Rpc.Promise      (Promise)
import qualified Capnp.Untyped          as U
import           Control.Concurrent.STM
import           Lifetimes
import           Zhp

newtype StoreAPI = StoreAPI Raw.Handler

newtype LiveRef = LiveRef KnownHash

new :: Raw.Handler -> Acquire StoreAPI
new h = pure (StoreAPI h)

liveRefHash :: LiveRef -> KnownHash
liveRefHash (LiveRef h) = h

getRoot :: StoreAPI -> Acquire (Promise LiveRef)
getRoot = undefined

getRef :: KnownHash -> StoreAPI -> Acquire (Promise (Maybe LiveRef))
getRef = undefined

put
    :: Maybe (U.Ptr 'Capnp.Const)
    -> (Client -> STM (Maybe KnownHash))
    -> StoreAPI
    -> Acquire (Promise LiveRef)
put = undefined

subscribeRoot :: (KnownHash -> STM ()) -> StoreAPI -> Acquire ()
subscribeRoot = undefined

setRoot :: Resource LiveRef -> StoreAPI -> STM (Promise ())
setRoot = undefined

checkpoint :: StoreAPI -> STM (Promise ())
checkpoint = undefined
