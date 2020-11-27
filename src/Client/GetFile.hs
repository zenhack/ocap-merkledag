{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Client.GetFile
    ( saveFile
    ) where

import Zhp

import           Capnp            (def)
import           Capnp.Rpc        ((?))
import qualified Capnp.Rpc        as Rpc
import qualified Data.ByteString  as BS
import qualified Data.Text        as T
import qualified Data.Vector      as V
import           System.Directory
    (createDirectory, createFileLink, doesPathExist, pathIsSymbolicLink)
import           System.FilePath  ((</>))

import Capnp.Gen.Files.Pure
import Capnp.Gen.Protocol.Pure

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
            -- I'm not 100% sure what doesPathExist does if the path
            -- is a symlink to something that doesn't exist... so we
            -- check for symlinks separately.
            existsSymlink <- pathIsSymbolicLink path'
            existsPath <- doesPathExist path'
            if existsSymlink || existsPath then
                pure $ Left $ AlreadyExists path'
            else
                case union' of
                    File'file File'file'{contents} ->
                        Right <$> saveRegularFile meta contents
                    File'dir kids ->
                        Right <$> saveDirectory meta kids
                    File'symlink target ->
                        Right <$> makeSymlink meta target
                    File'unknown' tag ->
                        pure $ Left $ UnknownFileType tag

saveRegularFile :: Metadata -> Ref BlobTree -> IO ()
saveRegularFile meta@Metadata{path} contents = do
    withBinaryFile path WriteMode $ \h ->
        putBlobTree (BS.hPut h) contents
    updateMetadata meta

putBlobTree :: (BS.ByteString -> IO ()) -> Ref BlobTree -> IO ()
putBlobTree putBytes ref = do
    Ref'get'results{value} <- Rpc.wait =<< ref'get ref ? def
    case value of
        BlobTree'leaf bytes ->
            putBytes bytes
        BlobTree'branch branches ->
            for_ branches $ \BlobTree'Branch{ref} ->
                putBlobTree putBytes ref

updateMetadata :: Metadata -> IO ()
updateMetadata Metadata{} = pure () -- TODO

saveDirectory :: Metadata -> Ref (V.Vector File) -> IO ()
saveDirectory meta@Metadata{path} ref = do
    createDirectory path
    Ref'get'results{value=files} <- Rpc.wait =<< ref'get ref ? def
    traverse_ (saveFile path) files
    updateMetadata meta

makeSymlink :: Metadata -> T.Text -> IO ()
makeSymlink Metadata{path} target =
    createFileLink (T.unpack target) path
