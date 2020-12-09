{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
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
-- The returned 'Ref BlobTree' is a promise that will be resolved when done()
-- is called on the stream.
--
-- The 'BlobTree' will be split as described in the documentation for
-- Store.putBytesStreaming.
makeStream :: Supervisor -> (BlobTree -> IO (Ref BlobTree)) -> IO (Util.ByteStream, Ref BlobTree)
makeStream sup putBlobTree = liftSTM $ do
    (p, f) <- Rpc.newPromiseClient
    state <- newTVar []
    stream <- Util.export_ByteStream sup Stream
        { putBlobTree
        , state
        , result = f
        }
    pure (stream, p)

type State = [BlobTree'Branch]

data Stream = Stream
    { putBlobTree :: BlobTree -> IO (Ref BlobTree)
    , result      :: Rpc.Fulfiller (Ref BlobTree)
    , state       :: TVar State
    }

instance Rpc.Server IO Stream

instance Util.ByteStream'server_ IO Stream where
    byteStream'expectSize = Rpc.pureHandler $
        \_ _ -> pure def

    byteStream'write = Rpc.pureHandler $
        \Stream{state, putBlobTree} Util.ByteStream'write'params{data_} -> do
            ref <- putBlobTree $ BlobTree'leaf data_
            let branch = BlobTree'Branch { ref, size = fromIntegral $ BS.length data_ }
            atomically $ modifyTVar' state (branch:)
            pure def

    byteStream'done = Rpc.pureHandler $
        \Stream{putBlobTree, state, result} _ -> do
            parts <- atomically $ readTVar state
            ref <- putBlobTree $ BlobTree'branch $ V.fromList $ reverse parts
            Rpc.fulfill result ref
            pure def

