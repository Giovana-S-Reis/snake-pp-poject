{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_testGloss (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/bin"
libdir     = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/lib/x86_64-linux-ghc-9.4.5/testGloss-0.1.0.0-CvL5iftOaLH66Tbidjcejt-testGloss"
dynlibdir  = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/share/x86_64-linux-ghc-9.4.5/testGloss-0.1.0.0"
libexecdir = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/libexec/x86_64-linux-ghc-9.4.5/testGloss-0.1.0.0"
sysconfdir = "/home/brunobrabello/Paradigmas/snake-pp-poject/.stack-work/install/x86_64-linux-tinfo6/b53e40dc80f4de5ce893a220f0f24b867a216c5b9534d02bae0b6c0ddcd95a35/9.4.5/etc"

getBinDir     = catchIO (getEnv "testGloss_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "testGloss_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "testGloss_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "testGloss_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testGloss_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testGloss_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
