{-# LANGUAGE LambdaCase #-}

import Control.Monad                   (when)
import Data.Char                       (toUpper)
import Data.Foldable                   (for_)
import Distribution.Compat.Time        (getModTime)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), emptyBuildInfo)
import Distribution.Simple
    (UserHooks(preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup       (BuildFlags(buildDistPref), fromFlag)
import System.Directory
    (createDirectoryIfMissing, findExecutable)
import System.Exit                     (die)
import System.Process                  (callProcess)

capnpFiles =
    [ "protocol"
    , "storage"
    , "util"
    ]

firstUpper :: String -> String
firstUpper ""     = ""
firstUpper (x:xs) = toUpper x : xs

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args buildFlags -> do
      capnp <- findExecutable "capnp" >>= \case
        Just path -> return path
        Nothing -> die "setup: Could not find executable capnp"

      let gensrc = fromFlag (buildDistPref buildFlags) ++ "/gensrc"
      createDirectoryIfMissing False gensrc
      for_ capnpFiles $ \file -> do
        let schemaFile = "schema/" ++ file ++ ".capnp"
            haskellFile = gensrc ++ "/Capnp/Gen/" ++ firstUpper file ++ ".hs"
        modTime_schema <- getModTime schemaFile
        modTime_gen <- getModTime haskellFile
        when (modTime_gen < modTime_schema) $
          callProcess capnp $
            ["compile", "-ohaskell:" ++ gensrc, "--src-prefix=schema/", schemaFile ]

      return (Just emptyBuildInfo{hsSourceDirs=[gensrc]}, [])
  }
