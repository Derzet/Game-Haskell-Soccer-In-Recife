module Paths_FunGEn (
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
version = Version [1,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/matheus/fungen/.cabal-sandbox/bin"
libdir     = "/home/matheus/fungen/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/FunGEn-1.0.1-K54PvKYUQsA6gmJ2twJm5Y"
datadir    = "/home/matheus/fungen/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/FunGEn-1.0.1"
libexecdir = "/home/matheus/fungen/.cabal-sandbox/libexec"
sysconfdir = "/home/matheus/fungen/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FunGEn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FunGEn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FunGEn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FunGEn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FunGEn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
