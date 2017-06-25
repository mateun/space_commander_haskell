module Paths_space_commander_haskell (
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

bindir     = "/home/martin/.cabal/bin"
libdir     = "/home/martin/.cabal/lib/x86_64-linux-ghc-7.10.3/space-commander-haskell-0.1.0.0-6bkCZfewR6HCUnay3X56q3"
datadir    = "/home/martin/.cabal/share/x86_64-linux-ghc-7.10.3/space-commander-haskell-0.1.0.0"
libexecdir = "/home/martin/.cabal/libexec"
sysconfdir = "/home/martin/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "space_commander_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "space_commander_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "space_commander_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "space_commander_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "space_commander_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
