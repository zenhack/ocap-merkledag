{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module LibMain (main) where

import Zhp

import Network.Simple.TCP (HostPreference(Host), ServiceName, connect, serve)

import BlobStore       (fromRaw)
import BlobStore.Files (open)
import Client          (storeFileRef)
import Server          (exportBlobStore)

import Capnp.Gen.Protocol.Pure

import Capnp     (def, defaultLimit)
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
        _                  -> do
            hPutStrLn stderr usageStr
            exitFailure

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
            { withBootstrap = Just $ \_sup store ->
                storeFileRef path (fromClient store) >>= print
            }
