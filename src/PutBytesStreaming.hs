{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
-- | This module implements the core logic for Protocol.putBytesStreaming.
--
-- N.B. right now this doesn't actually do hash splitting correctly,
-- it just puts each call to write() in a separate leaf. TODO: actually
-- implement hash splitting.
module PutBytesStreaming
    ( makeStream
    ) where

import Zhp

import Capnp     (def)
import Capnp.Rpc ((?))

import Capnp.Gen.Protocol.Pure
import Control.Concurrent.STM
import Control.Monad.STM.Class
import Supervisors

import qualified Capnp.Gen.Util.Pure as Util
import qualified Capnp.Rpc           as Rpc
import qualified Data.ByteString     as BS
import qualified Data.Vector         as V

-- | Create a stream client in which to write a large binary blob.
--
-- This does essentially the same thing as calling putBytesStreaming on the store,
-- but it does so only using the other methods on that interface. This can
-- either be used to do the chunking client-side, or as part of the server
-- implementation itself.
makeStream :: MonadSTM m => Supervisor -> Store BlobTree -> m (Util.ByteStream, Ref BlobTree)
makeStream sup store = liftSTM $ do
    (p, f) <- Rpc.newPromiseClient
    state <- newTVar []
    stream <- Util.export_ByteStream sup Stream
        { store
        , state
        , result = f
        }
    pure (stream, p)

type State = [BlobTree'Branch]

data Stream = Stream
    { store  :: Store BlobTree
    , result :: Rpc.Fulfiller (Ref BlobTree)
    , state  :: TVar State
    }

instance Rpc.Server IO Stream

instance Util.ByteStream'server_ IO Stream where
    byteStream'expectSize = Rpc.pureHandler $
        \_ _ -> pure def

    byteStream'write = Rpc.pureHandler $
        \Stream{state, store} Util.ByteStream'write'params{data_} -> do
            Store'put'results{ref, hash = _} <- Rpc.wait =<<
                store'put store ? Store'put'params
                    { value = BlobTree'leaf data_
                    }
            let branch = BlobTree'Branch { ref, size = fromIntegral $ BS.length data_ }
            atomically $ modifyTVar' state (branch:)
            pure def

    byteStream'done = Rpc.pureHandler $
        \Stream{store, state, result} _ -> do
            parts <- atomically $ readTVar state
            let blobTree = BlobTree'branch $ V.fromList $ reverse parts
            Store'put'results{ref, hash = _} <- Rpc.wait =<<
                store'put store ? Store'put'params { value = blobTree }
            Rpc.fulfill result ref
            pure def

