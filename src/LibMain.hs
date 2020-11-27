{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module LibMain (main) where

import Zhp

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS8
import           Network.Simple.TCP
    (HostPreference(Host), ServiceName, connect, serve)

import BlobStore       (KnownHash (..), decodeHash, fromRaw)
import BlobStore.Files (open)
import Client.GetFile  (downloadTree)
import Client.PutFile  (storeFileRef)
import Server          (exportBlobStore)

import Capnp.Gen.Protocol.Pure
import Crypto.Hash             (digestFromByteString)

import Capnp       (def, defaultLimit)
import Capnp.Rpc
    (ConnConfig (..), fromClient, handleConn, socketTransport, toClient)
import Supervisors (withSupervisor)

usageStr :: String
usageStr = mconcat
    [ "Usage:\n"
    , "\n"
    , "    omd serve <path-to-store> <host> <port>\n"
    , "    omd put <host> <port> <file-to-upload>\n"
    , "    omd help\n"
    ]

die str = do
    hPutStrLn stderr str
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["help"] ->
            putStrLn usageStr
        ["serve", path, host, port] ->
            server path (fromString host) (fromString port)
        ["put", host, port, path] ->
            putFile host (fromString port) path
        ["get", host, port, hash] ->
            getFile host port hash
        _                  ->
            die usageStr

server :: FilePath -> HostPreference -> ServiceName -> IO ()
server path host port = do
    store <- fromRaw <$> open path
    withSupervisor $ \sup -> do
        client <- toClient <$> exportBlobStore sup store
        serve host port $ \(sock, _addr) ->
            handleConn (socketTransport sock defaultLimit) def
                { getBootstrap = \_ -> pure $ Just client
                }

putFile :: String -> ServiceName -> FilePath -> IO ()
putFile host port path =
    connect host port $ \(sock, _remoteAddr) ->
        handleConn (socketTransport sock defaultLimit) def
            { withBootstrap = Just $ \_sup store -> do
                res <- storeFileRef path (fromClient store)
                case res of
                    Left e          -> print e
                    Right (hash, _) -> print $ decodeHash hash
            }

getFile :: String -> ServiceName -> String -> IO ()
getFile host port hash =
    case Base16.decode (BS8.pack hash) of
        Left e -> die e
        Right v -> case digestFromByteString v of
            Nothing -> die "Hash is the wrong length"
            Just digest ->
                connect host port $ \(sock, _remoteAddr) ->
                    handleConn (socketTransport sock defaultLimit) def
                        { withBootstrap = Just $ \_sup store ->
                            downloadTree "." (fromClient store) (Sha256 digest) >>= print
                        }
