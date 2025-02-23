{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ex1 (
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
version = Version [1,0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\bin"
libdir     = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\lib\\x86_64-windows-ghc-9.6.6\\ex1-1.0.0.0-5peGQSRZISxI4xywLYpFCm-ex1"
dynlibdir  = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\share\\x86_64-windows-ghc-9.6.6\\ex1-1.0.0.0"
libexecdir = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\libexec\\x86_64-windows-ghc-9.6.6\\ex1-1.0.0.0"
sysconfdir = "C:\\Users\\lucia\\Downloads\\3rdYear\\CSU34016_FunctionalProgramming\\.stack-work\\install\\feff23cc\\etc"

getBinDir     = catchIO (getEnv "ex1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ex1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ex1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ex1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex1_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
