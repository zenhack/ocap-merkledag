{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
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

import Capnp            (ReadParam, WriteParam, def)
import Control.Monad.ST (RealWorld)

import Capnp.Gen.Protocol.Pure
import Control.Concurrent.STM
import Control.Monad.STM.Class
import Supervisors

import qualified Capnp.Gen.Util.Pure as Util
import qualified Capnp.Rpc           as Rpc
import qualified Data.ByteString     as BS
import qualified Data.Vector         as V

type BlobPutFn =
    forall a. (ReadParam a, WriteParam RealWorld a)
    => a -> IO (Ref a)

-- | Create a stream client in which to write a large binary blob.
--
-- The returned 'Ref BlobTree' is a promise that will be resolved when done()
-- is called on the stream.
--
-- The 'BlobTree' will be split as described in the documentation for
-- Store.putBytesStreaming.
makeStream :: Supervisor -> BlobPutFn -> IO (Util.ByteStream, Ref BlobTree)
makeStream sup putBlob = liftSTM $ do
    (p, f) <- Rpc.newPromiseClient
    state <- newTVar []
    stream <- Util.export_ByteStream sup Stream
        { putBlob
        , state
        , result = f
        }
    pure (stream, p)

type State = [BlobTree]

data Stream = Stream
    { putBlob :: BlobPutFn
    , result  :: Rpc.Fulfiller (Ref BlobTree)
    , state   :: TVar State
    }

instance Rpc.Server IO Stream

instance Util.ByteStream'server_ IO Stream where
    byteStream'expectSize = Rpc.pureHandler $
        \_ _ -> pure def

    byteStream'write = Rpc.pureHandler $
        \Stream{state, putBlob} Util.ByteStream'write'params{data_} -> do
            ref <- putBlob data_
            let branch = BlobTree
                    { union' = BlobTree'leaf ref
                    , size = fromIntegral $ BS.length data_
                    }
            atomically $ modifyTVar' state (branch:)
            pure def

    byteStream'done = Rpc.pureHandler $
        \Stream{putBlob, state, result} _ -> do
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
