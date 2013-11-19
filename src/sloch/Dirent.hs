{-# LANGUAGE ScopedTypeVariables #-}

module Dirent
    ( Dirent(..)
    , direntsAtDepth
    , makeDirent
    , makeDirents
    ) where

import Control.Applicative  ((<$>))
import Control.Exception    (IOException, catch)
import Control.Monad.Extras (ifM)
import Data.Maybe           (catMaybes)

import System.Directory.Extras   (getDirectoryContents)
import System.FilePath.Extras    (isDotfile)
import System.Posix.Files.Extras (hasReadPermission, isDirectory, isSymbolicLink)

data Dirent = DirentDir FilePath [Dirent]
            | DirentFile FilePath
            deriving (Eq, Show)

-- | Build a Maybe Dirent from the specified FilePath. Returns Nothing if the specified file should be ignored. A file
-- should be ignored if:
--      - it's a symlink
--      - it's a dotfile, and @ignore_dotfiles@ is True.
--      - the file cannot be opened or does not exist.
-- Otherwise, return a Just DirentDir (recursively create Dirents from each entry, ignoring "." and "..") or a Just
-- DirentFile.
makeDirent :: Bool -> FilePath -> IO (Maybe Dirent)
makeDirent include_dotfiles path = catch makeDirent' (\(_ :: IOException) -> return Nothing)
  where
    makeDirent' :: IO (Maybe Dirent)
    makeDirent'
        | not include_dotfiles && isDotfile path = return Nothing
        | otherwise = do
            is_symlink <- isSymbolicLink path
            if is_symlink
                then return Nothing
                else do
                    can_read <- hasReadPermission path
                    if not can_read
                        then return Nothing
                        else fmap Just makeDirent''

    makeDirent'' :: IO Dirent
    makeDirent'' =
        ifM (isDirectory path)
            makeDirentDirectory
            (return $ DirentFile path)
      where
        makeDirentDirectory :: IO Dirent
        makeDirentDirectory = DirentDir path <$> makeDirentsFromChildren

        makeDirentsFromChildren :: IO [Dirent]
        makeDirentsFromChildren =
            catMaybes <$> (getDirectoryContents path >>= mapM (makeDirent include_dotfiles))

makeDirents :: Bool -> [FilePath] -> IO [Dirent]
makeDirents include_dotfiles = fmap catMaybes . mapM (makeDirent include_dotfiles)

-- | Transform a Dirent into a [Dirent], representing the entries at depth n from the provided dirent. Passing a
-- DirentFile into this function will result in []. A depth of zero means "this" level of depth.
direntsAtDepth :: Int -> Dirent -> [Dirent]
direntsAtDepth 0 d                         = [d]
direntsAtDepth _ d@(DirentFile _)          = [d]
direntsAtDepth n (DirentDir _ children) = concatMap (direntsAtDepth (n-1)) children
