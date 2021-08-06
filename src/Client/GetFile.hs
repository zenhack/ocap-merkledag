{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Client.GetFile
    ( saveFileRef
    , saveStoreRoot
    , downloadTree
    ) where

import Zhp

import           Capnp.New              (def)
import qualified Capnp.New              as C
import           Control.Exception.Safe (SomeException, try)
import qualified Data.ByteString        as BS
import           Data.Functor           ((<&>))
import qualified Data.Text              as T
import           System.Directory       (createDirectory, createFileLink)
import           System.FilePath        ((</>))
import qualified System.Posix.Files     as Posix
import qualified System.Posix.Types     as Posix

import Capnp.Gen.Files.New
import Capnp.Gen.Protocol.New

import BlobStore (KnownHash(..), encodeHash)

data SaveError
    = IllegalFileName T.Text
    | UnknownFileType !Word16
    | AlreadyExists FilePath
    deriving(Show)

data Metadata = Metadata
    { path        :: FilePath
    , permissions :: !Word32
    , modTime     :: !Int64
    }


getFileName :: Parsed File -> Either SaveError String
getFileName File{name}
    -- TODO: sanitize for windows paths?
    | name `elem` ["", ".", ".."] || '/' `elem` T.unpack name = Left $ IllegalFileName name
    | otherwise = Right (T.unpack name)

getPermissions :: Parsed File -> Word32
getPermissions File{permissions} = permissions .&. 0o777

downloadTree :: FilePath -> C.Client (Store File) -> KnownHash -> IO (Either SaveError ())
downloadTree path store hash = do
    ref <- store
        & C.callP #findByHash Store'findByHash'params { hash = encodeHash hash }
        <&> C.pipe #ref
        >>= C.asClient
    saveFileRef path ref

saveStoreRoot :: FilePath -> C.Client (Store File) -> IO (Either SaveError ())
saveStoreRoot path store = do
    ref <- store
        & C.callR #root def
        <&> C.pipe #root
        >>= C.callR #get def
        <&> C.pipe #value
        >>= C.asClient
    saveFileRef path ref

saveFileRef :: FilePath -> C.Client (Ref File) -> IO (Either SaveError ())
saveFileRef path ref = do
    value <- ref
        & C.callR #get def
        <&> C.pipe #value
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parse
    print ("saving file: ", value)
    saveFile path value

saveFile :: FilePath -> C.Parsed File -> IO (Either SaveError ())
saveFile path file@File{modTime, union'} = do
    case getFileName file of
        Left e -> pure $ Left e
        Right name -> do
            let path' = path </> name
                meta = Metadata
                    { path = path </> name
                    , permissions = getPermissions file
                    , modTime
                    }
            exists <- pathExists path'
            if exists then
                pure $ Left $ AlreadyExists path'
            else
                case union' of
                    File'file blobTree ->
                        Right <$> saveRegularFile meta blobTree
                    File'dir kids ->
                        Right <$> saveDirectory meta kids
                    File'symlink target ->
                        Right <$> makeSymlink meta target
                    File'unknown' tag ->
                        pure $ Left $ UnknownFileType tag

-- | Checks if a file exists at the given path. Unlike everything I can
-- find in libraries, this returns True even if the file at the path is
-- a dead symlink.
pathExists :: FilePath -> IO Bool
pathExists path = do
    res <- try (Posix.getSymbolicLinkStatus path)
    pure $ case res of
        Left (_ :: SomeException) -> False
        Right _                   -> True

saveRegularFile :: Metadata -> C.Client (Ref BlobTree) -> IO ()
saveRegularFile meta@Metadata{path} contentsRef = do
    contents <- contentsRef
        & C.callR #get def
        <&> C.pipe #value
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parse
    withBinaryFile path WriteMode $ \h ->
        putBlobTree (BS.hPut h) contents
    updateMetadata meta

putBlobTree :: (BS.ByteString -> IO ()) -> Parsed BlobTree -> IO ()
putBlobTree putBytes BlobTree {union'} = case union' of
    BlobTree'leaf bytesRef -> do
        bytes <- bytesRef
            & C.callR #get def
            <&> C.pipe #value
            >>= C.waitPipeline
            >>= C.evalLimitT C.defaultLimit . C.parse
        putBytes bytes
    BlobTree'branch branchesRef -> do
        branches <- branchesRef
            & C.callR #get def
            <&> C.pipe #value
            >>= C.waitPipeline
            >>= C.evalLimitT C.defaultLimit . C.parse
        traverse_ (putBlobTree putBytes) branches
    BlobTree'unknown' n ->
        error $ "Unknown BlobTree variant: " <> show n

updateMetadata :: Metadata -> IO ()
updateMetadata Metadata{path, permissions, modTime} = do
    Posix.setFileMode path (Posix.CMode permissions)
    status <- Posix.getFileStatus path
    Posix.setFileTimes path (Posix.accessTime status) (fromIntegral modTime)

saveDirectory :: Metadata -> C.Client (Ref (C.List File)) -> IO ()
saveDirectory meta@Metadata{path} ref = do
    createDirectory path
    files <- ref
        & C.callR #get def
        <&> C.pipe #value
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parse
    traverse_ (saveFile path) files
    updateMetadata meta

makeSymlink :: Metadata -> T.Text -> IO ()
makeSymlink Metadata{path} target =
    createFileLink (T.unpack target) path
