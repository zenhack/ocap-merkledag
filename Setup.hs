{-# LANGUAGE LambdaCase #-}

import Distribution.PackageDescription (BuildInfo(hsSourceDirs), emptyBuildInfo)
import Distribution.Simple
    (UserHooks(preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup       (BuildFlags(buildDistPref), fromFlag)
import System.Directory
    (createDirectoryIfMissing, findExecutable)
import System.Exit                     (die)
import System.Process                  (callProcess)

capnpFiles =
    [ "protocol.capnp"
    , "storage.capnp"
    , "util.capnp"
    , "files.capnp"
    ]

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args buildFlags -> do
      capnp <- findExecutable "capnp" >>= \case
        Just path -> return path
        Nothing -> die "setup: Could not find executable capnp"

      let gensrc = fromFlag (buildDistPref buildFlags) ++ "/gensrc"
      createDirectoryIfMissing False gensrc
      callProcess capnp $
        ["compile", "-ohaskell:" ++ gensrc, "--src-prefix=schema/" ] ++
        [ "schema/" ++ f | f <- capnpFiles ]

      return (Just emptyBuildInfo{hsSourceDirs=[gensrc]}, [])
  }
