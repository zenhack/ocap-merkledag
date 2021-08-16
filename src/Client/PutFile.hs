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

import qualified Capnp.Message as M
import           Capnp.New     (def)
import qualified Capnp.Rpc     as Rpc

import qualified Capnp.Gen.Files.New    as Files
import qualified Capnp.Gen.Protocol.New as P
import qualified Capnp.Gen.Util.New     as Util

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import qualified Capnp.New as N

data FileStoreError
    = ErrUnsupportedFileType
    deriving(Show)

type StoreResult a = Either FileStoreError a

storeFile :: FilePath -> N.Client (P.Store Files.File) -> IO (StoreResult (P.Parsed Files.File))
storeFile path store = do
    putStrLn $ "storeFile: " <> path
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

storeValue
    :: N.TypeParam a
    => N.Client (P.Store a)
    -> N.Parsed a
    -> IO (N.Pipeline P.Hash, N.Client (P.Ref a))
storeValue store value = do
    res <- N.callP #put P.Store'put'params { value } store
    ref <- N.asClient $ N.pipe #ref res
    pure
        ( N.pipe #hash res
        , ref
        )

setStoreRoot :: N.TypeParam a => N.Client (P.Store a) -> N.Client (P.Ref a) -> IO ()
setStoreRoot store ref = do
    void $ store
        & N.callR #root def
        <&> N.pipe #root
        >>= N.callR #asSetter def
        <&> N.pipe #setter
        >>= N.callP #set Util.Assignable'Setter'set'params { value = ref }
        >>= N.waitPipeline

storeFileRef
    :: FilePath
    -> N.Client (P.Store Files.File)
    -> IO (StoreResult (N.Pipeline P.Hash, N.Client (P.Ref Files.File)))
storeFileRef path store =
    storeFile path store >>= traverse (storeValue store)

castStore :: N.Client (P.Store a) -> N.Client (P.Store b)
castStore = Rpc.fromClient . Rpc.toClient

storeFileUnion
    :: Posix.FileStatus
    -> FilePath
    -> N.Client (P.Store Files.File)
    -> IO (StoreResult (P.Parsed (N.Which Files.File)))
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


storeHandleBlob :: N.Client (P.Store Files.File) -> Handle -> IO (N.Client (P.Ref P.BlobTree))
storeHandleBlob store h = do
    res <- store & N.callR #putBytesStreaming def
    _size <- res
        & N.pipe #stream
        & N.asClient
        >>= streamHandle h
    res
        & N.pipe #ref
        & N.asClient

streamHandle :: Handle -> N.Client Util.ByteStream -> IO Word64
streamHandle h stream = go 0
  where
    go !size = do
        bytes <- BS.hGet h blockSize
        if bytes == BS.empty
            then (do
                _ <- stream & N.callR #done def
                pure size)
            else (do
                _ <- stream & N.callB #write (do
                    msg <- M.newMessage Nothing
                    params <- N.newRoot @Util.ByteStream'write'params () msg
                    N.encodeField #data_ bytes params
                    pure params)
                go $ size + fromIntegral (BS.length bytes))

-- | Relatively arbitrary size of a chunk to upload at a time.
-- currently 64KiB.
blockSize :: Int
blockSize = 64 * 1024
