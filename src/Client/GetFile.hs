{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Client.GetFile
    ( saveFileRef
    , saveStoreRoot
    , downloadTree
    ) where

import Zhp

import           Capnp                  (def)
import           Capnp.Rpc              ((?))
import qualified Capnp.Rpc              as Rpc
import           Control.Exception.Safe (SomeException, try)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           System.Directory       (createDirectory, createFileLink)
import           System.FilePath        ((</>))
import qualified System.Posix.Files     as Posix
import qualified System.Posix.Types     as Posix

import           Capnp.Gen.Files.Pure
import           Capnp.Gen.Protocol.Pure
import qualified Capnp.Gen.Util.Pure     as Util

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


getFileName :: File -> Either SaveError String
getFileName File{name}
    -- TODO: sanitize for windows paths?
    | name `elem` ["", ".", ".."] || '/' `elem` T.unpack name = Left $ IllegalFileName name
    | otherwise = Right (T.unpack name)

getPermissions :: File -> Word32
getPermissions File{permissions} = permissions .&. 0o777

downloadTree :: FilePath -> Store File -> KnownHash -> IO (Either SaveError ())
downloadTree path store hash = do
    Store'findByHash'results{ref} <- Rpc.wait =<<
        store'findByHash store ? Store'findByHash'params { hash = encodeHash hash }
    saveFileRef path ref

saveStoreRoot :: FilePath -> Store File -> IO (Either SaveError ())
saveStoreRoot path store = do
    Store'root'results{root} <- Rpc.wait =<< store'root store ? def
    Util.Assignable'get'results{value} <- Rpc.wait =<< Util.assignable'get root ? def
    saveFileRef path value

saveFileRef :: FilePath -> Ref File -> IO (Either SaveError ())
saveFileRef path ref = do
    Ref'get'results{value} <- Rpc.wait =<< ref'get ref ? def
    print ("saving file: ", value)
    saveFile path value

saveFile :: FilePath -> File -> IO (Either SaveError ())
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

saveRegularFile :: Metadata -> Ref BlobTree -> IO ()
saveRegularFile meta@Metadata{path} contentsRef = do
    Ref'get'results contents <- Rpc.wait =<< ref'get contentsRef ? def
    withBinaryFile path WriteMode $ \h ->
        putBlobTree (BS.hPut h) contents
    updateMetadata meta

putBlobTree :: (BS.ByteString -> IO ()) -> BlobTree -> IO ()
putBlobTree putBytes BlobTree {union'} = case union' of
    BlobTree'leaf bytesRef -> do
        Ref'get'results bytes <- Rpc.wait =<< ref'get bytesRef ? def
        putBytes bytes
    BlobTree'branch branchesRef -> do
        Ref'get'results branches <- Rpc.wait =<< ref'get branchesRef ? def
        traverse_ (putBlobTree putBytes) branches
    BlobTree'unknown' n ->
        error $ "Unknown BlobTree variant: " <> show n

updateMetadata :: Metadata -> IO ()
updateMetadata Metadata{path, permissions, modTime} = do
    Posix.setFileMode path (Posix.CMode permissions)
    status <- Posix.getFileStatus path
    Posix.setFileTimes path (Posix.accessTime status) (fromIntegral modTime)

saveDirectory :: Metadata -> Ref (V.Vector File) -> IO ()
saveDirectory meta@Metadata{path} ref = do
    createDirectory path
    Ref'get'results{value=files} <- Rpc.wait =<< ref'get ref ? def
    traverse_ (saveFile path) files
    updateMetadata meta

makeSymlink :: Metadata -> T.Text -> IO ()
makeSymlink Metadata{path} target =
    createFileLink (T.unpack target) path
