{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_helio_bot (
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

bindir     = "/home/aiden/.cabal/bin"
libdir     = "/home/aiden/.cabal/lib/x86_64-linux-ghc-8.6.5/helio-bot-0.1.0.0-inplace-helio-bot"
dynlibdir  = "/home/aiden/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/aiden/.cabal/share/x86_64-linux-ghc-8.6.5/helio-bot-0.1.0.0"
libexecdir = "/home/aiden/.cabal/libexec/x86_64-linux-ghc-8.6.5/helio-bot-0.1.0.0"
sysconfdir = "/home/aiden/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "helio_bot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "helio_bot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "helio_bot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "helio_bot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "helio_bot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "helio_bot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
