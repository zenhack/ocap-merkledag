{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
-- | This module implements the core logic for Protocol.putBytesStreaming.
--
-- N.B. right now this doesn't actually do hash splitting correctly,
-- it just puts each call to write() in a separate leaf. TODO: actually
-- implement hash splitting.
module PutBytesStreaming
    ( makeStream
    , BlobPutFn
    ) where

import Zhp

import           Capnp     (def)
import qualified Capnp.New as Capnp

import Capnp.Gen.Protocol.New
import Control.Concurrent.STM
import Control.Monad.STM.Class
import Supervisors

import qualified Capnp.Gen.Util.New as Util
import qualified Capnp.Rpc          as Rpc
import qualified Data.ByteString    as BS
import qualified Data.Vector        as V

type BlobPutFn =
    forall a. Capnp.TypeParam a => Capnp.Parsed a -> IO (Capnp.Client (Ref a))

-- | Create a stream client in which to write a large binary blob.
--
-- The returned 'Ref BlobTree' is a promise that will be resolved when done()
-- is called on the stream.
--
-- The 'BlobTree' will be split as described in the documentation for
-- Store.putBytesStreaming.
makeStream :: Supervisor -> BlobPutFn -> IO (Capnp.Client Util.ByteStream, Capnp.Client (Ref BlobTree))
makeStream sup putBlob = liftSTM $ do
    (p, f) <- Rpc.newPromiseClient
    state <- newTVar []
    stream <- Capnp.export @Util.ByteStream sup Stream
        { putBlob
        , state
        , result = f
        }
    pure (stream, p)

type State = [Capnp.Parsed BlobTree]

data Stream = Stream
    { putBlob :: BlobPutFn
    , result  :: Rpc.Fulfiller (Capnp.Client (Ref BlobTree))
    , state   :: TVar State
    }

instance Capnp.SomeServer Stream

instance Util.ByteStream'server_ Stream where
    byteStream'expectSize _ = Capnp.handleRaw $ \_ -> pure def

    byteStream'write Stream{state, putBlob} =
        Capnp.handleParsed $ \Util.ByteStream'write'params{data_} -> do
            ref <- putBlob data_
            let branch = BlobTree
                    { union' = BlobTree'leaf ref
                    , size = fromIntegral $ BS.length data_
                    }
            atomically $ modifyTVar' state (branch:)
            pure def

    byteStream'done Stream{putBlob, state, result} =
        Capnp.handleParsed $ \_ -> do
            parts <- atomically $ readTVar state
            let totalSize = sum [ size | BlobTree {size} <- parts ]
            -- TODO: if there is only one block, skip the wrapper node.
            branchesRef <- putBlob $ V.fromList $ reverse parts
            ref <- putBlob $ BlobTree
                { size = totalSize
                , union' = BlobTree'branch branchesRef
                }
            Rpc.fulfill result ref
            pure def
