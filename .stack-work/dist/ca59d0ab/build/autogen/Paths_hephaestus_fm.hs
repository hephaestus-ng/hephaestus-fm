{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hephaestus_fm (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\bin"
libdir     = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\lib\\x86_64-windows-ghc-8.0.2\\hephaestus-fm-0.1.0.0-BzoirjheX0ZB6OJEKgwIRT"
dynlibdir  = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\share\\x86_64-windows-ghc-8.0.2\\hephaestus-fm-0.1.0.0"
libexecdir = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\libexec"
sysconfdir = "C:\\Users\\Pichau\\git\\hephaestus-fm\\.stack-work\\install\\5600629f\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hephaestus_fm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hephaestus_fm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hephaestus_fm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hephaestus_fm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hephaestus_fm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hephaestus_fm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
