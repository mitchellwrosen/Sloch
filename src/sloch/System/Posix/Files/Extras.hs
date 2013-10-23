module System.Posix.Files.Extras where

import qualified System.Posix.Files as F

isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = fmap F.isSymbolicLink . F.getSymbolicLinkStatus

isDirectory :: FilePath -> IO Bool
isDirectory = fmap F.isDirectory . F.getFileStatus
