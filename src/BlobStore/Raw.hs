-- | Raw interface exposed by low level data stores.
--
-- The types in this module define a protocol for communicating with a _low level_
-- data store.
--
-- Notably, the types in the protocol do *not* enforce correct use of the protocol,
-- for performance reasons. These invariants instead are maintained by
-- "BlobStore.HighLevel", which is the only code that should be directly talking to
-- low-level data stores.
{-# LANGUAGE DataKinds #-}
module BlobStore.Raw
    ( Request(..)
    , SubscribeRootRequest(..)
    , PutRequest(..)
    , Handler
    ) where

import           BlobStore
import qualified Capnp
import           Capnp.Rpc.Promise      (Fulfiller)
import           Control.Concurrent.STM
import           Lifetimes              (Resource)
import           Zhp

-- | A 'Handler' submits requests to the data store. It may return
-- before the the request has actually been handled.
type Handler = Request -> STM ()

-- | A request for some action from the data store.
data Request
    = Put PutRequest
    -- ^ Insert a blob into the data store, if it does not
    -- already exist.
    | Checkpoint (Fulfiller ())
    -- ^ Ensure changes made to the root object by prior requests are
    -- durable, i.e. they will persist in the event of a power failure
    -- or the like.
    | GetRoot (Fulfiller (Resource KnownHash))
    -- ^ Get the curent root object. The while the Resource is alive,
    -- it keeps the object from from being garbage collected, even if
    -- the root is changed to point to something else.
    | GetRef KnownHash (Fulfiller (Maybe (Resource KnownHash)))
    -- ^ Get a reference to the object with the given hash, if it exists
    -- in the store (otherwise the request is fulfilled with 'Nothing').
    | SubscribeRoot SubscribeRootRequest
    -- ^ Subscribe to changes to the root object; see 'SubscribeRootRequest'
    -- for more details.
    | SetRoot KnownHash (Fulfiller ())
    -- ^ Change the root object to point to the given hash, which the requester
    -- must ensure is already present in the store, and cannot be garbage
    -- collected before this request has completed.

-- | A request to subscribe to changes to the root object.
data SubscribeRootRequest = SubscribeRootRequest
    { onUpdate :: Resource KnownHash -> STM ()
    -- ^ Invoke this once when the request is received, and again whenever the
    -- root object changes. The argument is a reference to the new root object,
    -- which acts as a GC root.
    , canceler :: Fulfiller (Resource ())
    -- ^ Fulfilled when the request is received. The resource represents the
    -- description; when it is dropped onUpdate will no longer be called.
    }

-- | A request to save a value in the store.
data PutRequest = PutRequest
    { msg    :: Capnp.Message 'Capnp.Const
    -- ^ The capnp message to save. This should be an already-canonicalized
    -- (and single segment) message, with a StoredBlob as its root pointer.
    , hash   :: KnownHash
    -- ^ The hash of the canonicalized message (without the message header).
    , refs   :: [KnownHash]
    -- ^ The blobs referenced by this message. This should correspond exactly
    -- to the `ptr` field in the StoredBlob.
    , result :: Fulfiller (Resource ())
    -- ^ fulfilled when the request has completed, though not necessarily
    -- durably (it may be rolled back if e.g. a power failure occurs). The
    -- resource represents a live reference to this blob, which will act
    -- as a GC root for the store; dropping the resource will allow the
    -- blob to be reclaimed by store's garbage collector.
    }
