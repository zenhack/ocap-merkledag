{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module LibMain (main) where

import Zhp

import Network.Simple.TCP (HostPreference, ServiceName, connect, serve)

-- import BlobStore      (decodeHash)
import Client.GetFile (saveStoreRoot)
import Client.PutFile (setStoreRoot, storeFileRef)

import           Capnp              (def, defaultLimit, evalLimitT)
import qualified Capnp.New.Classes  as NC
import           Capnp.Repr.Methods (waitPipeline)
import           Capnp.Rpc
    (ConnConfig(..), fromClient, handleConn, socketTransport, toClient)
import           Supervisors        (Supervisor, withSupervisor)

import qualified BlobStore.InMemory      as InMemory
import qualified Capnp.Gen.Protocol.Pure as Protocol
import qualified Capnp.Untyped.Pure      as PU
import qualified Server2

import qualified Lifetimes

usageStr :: String
usageStr = mconcat
    [ "Usage:\n"
    , "\n"
    , "    omd serve <path-to-store> <host> <port>\n"
    , "    omd put <host> <port> <file-to-upload>\n"
    , "    omd get <host> <port> <save-path>\n"
    , "    omd help\n"
    ]

die :: String -> IO ()
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
        ["get", host, port, path] ->
            getFile host port path
        _                  ->
            die usageStr

server :: FilePath -> HostPreference -> ServiceName -> IO ()
server _path host port = do
    withSupervisor $ \sup ->
        Lifetimes.withAcquire (acquireServer sup) $ \client -> do
            serve host port $ \(sock, _addr) ->
                handleConn (socketTransport sock defaultLimit) def
                    { getBootstrap = \_ -> pure $ Just (toClient client)
                    }

acquireServer :: Supervisor -> Lifetimes.Acquire (Protocol.Store (Maybe PU.Ptr))
acquireServer sup = do
    h <- InMemory.acquireHandler
    srv <- Server2.acquireStoreServer sup h
    liftIO $ Protocol.export_Store sup srv

putFile :: String -> ServiceName -> FilePath -> IO ()
putFile host port path =
    connect host port $ \(sock, _remoteAddr) ->
        handleConn (socketTransport sock defaultLimit) def
            { withBootstrap = Just $ \_sup store -> do
                res <- storeFileRef path (fromClient store)
                case res of
                    Left e          -> print e
                    Right (hashPipe, ref) -> do
                        rawHash <- waitPipeline hashPipe
                        -- arbitrary limit; hashes aren't huge, so this
                        -- should always be sufficient.
                        hash <- evalLimitT 200 $ NC.parse rawHash
                        -- print (decodeHash hash)
                        print hash
                        setStoreRoot (fromClient store) ref
            }

getFile :: String -> ServiceName -> String -> IO ()
getFile host port path =
    connect host port $ \(sock, _remoteAddr) ->
        handleConn (socketTransport sock defaultLimit) def
            { withBootstrap = Just $ \_sup store ->
                saveStoreRoot path (fromClient store) >>= print
            }
