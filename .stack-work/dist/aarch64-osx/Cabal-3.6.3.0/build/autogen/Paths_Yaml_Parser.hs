{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Yaml_Parser (
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
bindir     = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/bin"
libdir     = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/lib/aarch64-osx-ghc-9.2.5/Yaml-Parser-0.1.0.0-CQdaiumzT06Cevy6muZ6JX"
dynlibdir  = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/lib/aarch64-osx-ghc-9.2.5"
datadir    = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/share/aarch64-osx-ghc-9.2.5/Yaml-Parser-0.1.0.0"
libexecdir = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/libexec/aarch64-osx-ghc-9.2.5/Yaml-Parser-0.1.0.0"
sysconfdir = "/Users/domantelio/Projects/Yaml-Parser/.stack-work/install/aarch64-osx/70349613089a66f53b3ab505433134ed8fea90f90a3ea8d50a585a4881b47b66/9.2.5/etc"

getBinDir     = catchIO (getEnv "Yaml_Parser_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Yaml_Parser_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Yaml_Parser_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Yaml_Parser_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Yaml_Parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Yaml_Parser_sysconfdir") (\_ -> return sysconfdir)




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
