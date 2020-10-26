{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module LibMain (main) where

import Zhp

import Network.Simple.TCP (HostPreference(Host), ServiceName, serve)

import BlobStore       (fromRaw)
import BlobStore.Files (open)
import Server          (StoreServer (..))

import Capnp.Gen.Protocol.Pure

import Capnp       (def, defaultLimit)
import Capnp.Rpc   (ConnConfig (..), handleConn, socketTransport, toClient)
import Supervisors (withSupervisor)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["serve", path, host, port] -> server path (fromString host) (fromString port)
        _                  -> do
            hPutStrLn stderr "Usage: omd serve <path> <host> <port>"
            exitFailure

server :: FilePath -> HostPreference -> ServiceName -> IO ()
server path host port = do
    store <- fromRaw <$> open path
    withSupervisor $ \sup -> do
        client <- toClient <$> export_Store sup StoreServer { store, sup }
        serve host port $ \(sock, _addr) ->
            handleConn (socketTransport sock defaultLimit) def
                { getBootstrap = \_ -> pure $ Just client
                }
