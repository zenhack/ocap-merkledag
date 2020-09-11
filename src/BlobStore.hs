module BlobStore where

import Zhp

import Capnp.Gen.Protocol.Pure
import Capnp.Gen.Storage.Pure

data BlobStore m = BlobStore
    { getBlob :: Hash -> m StoredBlob
    , putBlob :: StoredBlob -> m Hash
    }
