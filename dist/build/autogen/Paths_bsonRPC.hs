module Paths_bsonRPC (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/rick/.cabal/bin"
libdir     = "/home/rick/.cabal/lib/bsonRPC-0.0.1/ghc-6.10.4"
datadir    = "/home/rick/.cabal/share/bsonRPC-0.0.1"
libexecdir = "/home/rick/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bsonRPC_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bsonRPC_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bsonRPC_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bsonRPC_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
