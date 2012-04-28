module Paths_hotswap (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,6,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/opuk/.cabal/bin"
libdir     = "/home/opuk/.cabal/lib/hotswap-0.1.6.0/ghc-7.4.1"
datadir    = "/home/opuk/.cabal/share/hotswap-0.1.6.0"
libexecdir = "/home/opuk/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hotswap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hotswap_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hotswap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hotswap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
