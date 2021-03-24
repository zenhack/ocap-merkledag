module Main (main) where

import qualified CanonicalizeBytes
import qualified Capnp
import qualified Capnp.Canonicalize        as C
import           Capnp.Classes             (toStruct)
import           Capnp.Gen.Protocol.Pure
import           Capnp.Gen.Storage.Pure
import qualified Capnp.Message             as M
import qualified Data.ByteString           as BS
import qualified Data.Vector               as V
import           Test.Hspec
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Instances ()
import           Zhp

prop_canonicalizeBytesEquiv :: BS.ByteString -> Bool
prop_canonicalizeBytesEquiv bytes =
    let value = StoredBlob
            { data_ = BlobTree'leaf bytes
            , ptrs = V.empty
            }
        Just normalCanonicalized =
            Capnp.msgToLBS <$> Capnp.createPure maxBound (do
                msg <- M.newMessage Nothing
                structIn <- Capnp.cerialize msg value
                (msg', _seg') <- C.canonicalize (toStruct structIn)
                pure msg')
        fastCanonicalized = CanonicalizeBytes.canonicalizeBytesBlob bytes
    in
    normalCanonicalized == fastCanonicalized



main :: IO ()
main = hspec $ do
    describe "canonicalizeBytes" $ do
        it "Should be equivalent to canonicalizing the corresponding BlobTree" $ do
            property prop_canonicalizeBytesEquiv
