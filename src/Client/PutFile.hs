{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Client.PutFile
    ( storeFileRef
    , setStoreRoot
    , StoreResult
    , FileStoreError(..)
    ) where

import Zhp

import Data.Functor     ((<&>))
import System.Directory (listDirectory)
import System.FilePath  (takeFileName, (</>))
import System.IO        (withFile)

import Foreign.C.Types (CTime(..))

import qualified System.Posix.Files as Posix

import           Capnp         (def)
import qualified Capnp.Message as M
import qualified Capnp.Rpc     as Rpc

import qualified Capnp.Gen.Files.New    as Files
import qualified Capnp.Gen.Protocol.New as P
import qualified Capnp.Gen.Util.New     as Util

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import           Capnp.Fields       (HasUnion(..))
import qualified Capnp.New          as N
import qualified Capnp.New.Classes  as NC
import qualified Capnp.Repr.Methods as RM
import qualified Capnp.Repr.Parsed  as RP

data FileStoreError
    = ErrUnsupportedFileType
    deriving(Show)

type StoreResult a = Either FileStoreError a

storeFile :: FilePath -> RM.Client (P.Store Files.File) -> IO (StoreResult (P.Parsed Files.File))
storeFile path store = do
    status <- Posix.getSymbolicLinkStatus path
    let CTime modTime = Posix.modificationTime status
    storeFileUnion status path store >>= \case
        Left err -> pure $ Left err
        Right union' -> pure $ Right Files.File
            { Files.name = fromString $ takeFileName path
            , Files.modTime = modTime
            , Files.permissions = fromIntegral (Posix.fileMode status) .&. 0o777
            , Files.union' = union'
            }

storeValue ::
    ( NC.Parse a (RP.Parsed a)
    , N.TypeParam a _pr
    )
    => RM.Client (P.Store a)
    -> RP.Parsed a
    -> IO (RM.Pipeline P.Hash, RM.Client (P.Ref a))
storeValue store value = do
    res <- RM.callP #put P.Store'put'params { value } store
    ref <- RM.asClient $ RM.pipe #ref res
    pure
        ( RM.pipe #hash res
        , ref
        )

setStoreRoot ::
    ( NC.Parse a (RP.Parsed a)
    , N.TypeParam a _pr
    )
    => RM.Client (P.Store a) -> RM.Client (P.Ref a) -> IO ()
setStoreRoot store ref = do
    void $ store
        & RM.callR #root def
        <&> RM.pipe #root
        >>= RM.callR #asSetter def
        <&> RM.pipe #setter
        >>= RM.callP #set Util.Assignable'Setter'set'params { value = ref }
        >>= RM.waitPipeline

storeFileRef
    :: FilePath
    -> RM.Client (P.Store Files.File)
    -> IO (StoreResult (RM.Pipeline P.Hash, RM.Client (P.Ref Files.File)))
storeFileRef path store =
    storeFile path store >>= traverse (storeValue store)

castStore :: RM.Client (P.Store a) -> RM.Client (P.Store b)
castStore = Rpc.fromClient . Rpc.toClient

storeFileUnion
    :: Posix.FileStatus
    -> FilePath
    -> RM.Client (P.Store Files.File)
    -> IO (StoreResult (P.Parsed (Which Files.File)))
storeFileUnion status path store =
    if Posix.isRegularFile status then do
        content <- withFile path ReadMode (storeHandleBlob store)
        pure $ Right $ Files.File'file content
    else if Posix.isSymbolicLink status then do
        target <- Posix.readSymbolicLink path
        pure $ Right $ Files.File'symlink (fromString target)
    else if Posix.isDirectory status then do
        files <- listDirectory path
        results <- for files $ \file ->
            storeFile (path </> file) store
        let refs = flip mapMaybe results $ \case
                Left _  -> Nothing
                Right v -> Just v
        (_, ref) <- storeValue (castStore store) (V.fromList refs)
        pure $ Right $ Files.File'dir ref
    else
        pure $ Left ErrUnsupportedFileType


storeHandleBlob :: RM.Client (P.Store Files.File) -> Handle -> IO (RM.Client (P.Ref P.BlobTree))
storeHandleBlob store h = do
    res <- store & RM.callR #putBytesStreaming def
    _size <- res
        & RM.pipe #stream
        & RM.asClient
        >>= streamHandle h
    res
        & RM.pipe #ref
        & RM.asClient

streamHandle :: Handle -> RM.Client Util.ByteStream -> IO Word64
streamHandle h stream = go 0
  where
    go !size = do
        bytes <- BS.hGet h blockSize
        if bytes == BS.empty
            then (do
                _ <- stream & RM.callR #done def
                pure size)
            else (do
                _ <- stream & RM.callB #write (do
                    msg <- M.newMessage Nothing
                    params <- NC.newRoot @Util.ByteStream'write'params () msg
                    N.encodeField #data_ bytes params
                    pure params)
                go $ size + fromIntegral (BS.length bytes))

-- | Relatively arbitrary size of a chunk to upload at a time.
-- currently 64KiB.
blockSize :: Int
blockSize = 64 * 1024
