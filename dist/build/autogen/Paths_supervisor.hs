module Paths_supervisor (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\supervisor-0.1.0.0\\ghc-7.4.2"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\supervisor-0.1.0.0"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\supervisor-0.1.0.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "supervisor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "supervisor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "supervisor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "supervisor_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)