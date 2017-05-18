module Paths_Chapters (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/meriy100/.cabal/bin"
libdir     = "/Users/meriy100/.cabal/lib/x86_64-osx-ghc-7.10.2/Chapters-0.1.0.0-6EleRJKXWuCAwHiwO6nGF4"
datadir    = "/Users/meriy100/.cabal/share/x86_64-osx-ghc-7.10.2/Chapters-0.1.0.0"
libexecdir = "/Users/meriy100/.cabal/libexec"
sysconfdir = "/Users/meriy100/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Chapters_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Chapters_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Chapters_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Chapters_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Chapters_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
