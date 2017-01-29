module Paths_haskell_house_chores (
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

bindir     = "/home/jerry/Documents/haskell-house-chores/.cabal-sandbox/bin"
libdir     = "/home/jerry/Documents/haskell-house-chores/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/haskell-house-chores-0.1.0.0-CSH21nSbG7IIpkNEIQ6ozJ"
datadir    = "/home/jerry/Documents/haskell-house-chores/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/haskell-house-chores-0.1.0.0"
libexecdir = "/home/jerry/Documents/haskell-house-chores/.cabal-sandbox/libexec"
sysconfdir = "/home/jerry/Documents/haskell-house-chores/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_house_chores_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_house_chores_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_house_chores_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_house_chores_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_house_chores_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
