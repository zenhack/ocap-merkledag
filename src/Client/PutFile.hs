{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
module Client.PutFile
    ( storeFileRef
    , setStoreRoot
    , StoreResult
    , FileStoreError(..)
    ) where

import Zhp

import Control.Monad.ST (RealWorld)
import System.Directory (listDirectory)
import System.FilePath  (takeFileName, (</>))
import System.IO        (withFile)

import Foreign.C.Types (CTime(..))

import qualified System.Posix.Files as Posix

import           Capnp         (def)
import qualified Capnp
import qualified Capnp.Message as M
import           Capnp.Rpc     ((?))
import qualified Capnp.Rpc     as Rpc

import qualified Capnp.Gen.Files.Pure    as Files
import qualified Capnp.Gen.Protocol.New  as PN
import qualified Capnp.Gen.Protocol.Pure as P
import qualified Capnp.Gen.Util.New      as UN
import qualified Capnp.Gen.Util.Pure     as Util

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import qualified Capnp.New          as N
import qualified Capnp.New.Classes  as NC
import qualified Capnp.Repr.Methods as RM

data FileStoreError
    = ErrUnsupportedFileType
    deriving(Show)

type StoreResult a = Either FileStoreError a

storeFile :: FilePath -> P.Store Files.File -> IO (StoreResult Files.File)
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

storeValue :: (Capnp.ReadParam a, Capnp.WriteParam RealWorld a) => P.Store a -> a -> IO (P.Hash, P.Ref a)
storeValue store value = do
    P.Store'put'results{hash, ref} <-
        Rpc.wait =<< P.store'put store ? P.Store'put'params { value }
    pure (hash, ref)

setStoreRoot :: (Capnp.ReadParam a, Capnp.WriteParam RealWorld a) => P.Store a -> P.Ref a -> IO ()
setStoreRoot store ref = do
    P.Store'root'results{root} <- Rpc.wait =<< P.store'root store ? def
    Util.Assignable'asSetter'results{setter} <- Rpc.wait =<< Util.assignable'asSetter root ? def
    _ <- Rpc.wait =<< Util.assignable'Setter'set setter ? Util.Assignable'Setter'set'params { value = ref }
    pure ()

storeFileRef :: FilePath -> P.Store Files.File -> IO (StoreResult (P.Hash, P.Ref Files.File))
storeFileRef path store =
    storeFile path store >>= traverse (storeValue store)

castStore :: P.Store a -> P.Store b
castStore = Rpc.fromClient . Rpc.toClient

storeFileUnion
    :: Posix.FileStatus
    -> FilePath
    -> P.Store Files.File
    -> IO (StoreResult Files.File')
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


storeHandleBlob :: P.Store Files.File -> Handle -> IO (P.Ref P.BlobTree)
storeHandleBlob store h = do
    res <- (RM.Client (Rpc.toClient store) :: RM.Client (PN.Store PN.BlobTree))
        & RM.callR #putBytesStreaming def
    _size <- res
        & RM.pipe #stream
        & RM.asClient
        >>= streamHandle h
    res
        & RM.pipe #ref
        & RM.asClient
        >>= pure . Rpc.fromClient . Rpc.toClient

streamHandle :: Handle -> RM.Client UN.ByteStream -> IO Word64
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
                    params <- NC.newRoot @UN.ByteStream'write'params () msg
                    N.encodeField #data_ bytes params
                    pure params)
                go $ size + fromIntegral (BS.length bytes))

-- | Relatively arbitrary size of a chunk to upload at a time.
-- currently 64KiB.
blockSize :: Int
blockSize = 64 * 1024
