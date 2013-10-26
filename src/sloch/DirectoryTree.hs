{-# LANGUAGE RankNTypes, TupleSections #-}

module DirectoryTree
    ( Dirent(..)
    , direntsAtDepth
    , makeDirent
    , makeDirents
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Extras (ifM)
import Data.Maybe (catMaybes)

import System.Directory.Extras (getDirectoryContents)
import System.FilePath.Extras (isDotfile)
import System.Posix.Files.Extras (isDirectory, isSymbolicLink)

data Dirent = DirentDir FilePath [Dirent]
            | DirentFile FilePath
            deriving Show

makeDirents :: [FilePath] -> Bool -> IO [Dirent]
makeDirents paths include_dotfiles = fmap catMaybes $ mapM (\p -> makeDirent p include_dotfiles) paths

-- | Build a Maybe Dirent from the specified FilePath. Returns Nothing if the specified file should be ignored. A file
-- should be ignored if:
--      - it's a symlink
--      - it's a dotfile, and @ignore_dotfiles@ is True.
--      - the file cannot be opened or does not exist.
-- Otherwise, return a Just DirentDir (recursively create Dirents from each entry, ignoring "." and "..") or a Just
-- DirentFile.
makeDirent :: FilePath -> Bool -> IO (Maybe Dirent)
makeDirent path include_dotfiles
    | not include_dotfiles && isDotfile path = return Nothing
    | otherwise = ifM (isSymbolicLink path)
                      (return Nothing)
                      (fmap Just makeDirent')
  where
    makeDirent' :: IO Dirent
    makeDirent' =
        ifM (isDirectory path)
            makeDirentDirectory
            (return $ DirentFile path)
      where
        makeDirentDirectory :: IO Dirent
        makeDirentDirectory = DirentDir path <$> makeDirentsFromChildren

        makeDirentsFromChildren :: IO [Dirent]
        makeDirentsFromChildren =
            catMaybes <$> (getDirectoryContents path >>= mapM (`makeDirent` include_dotfiles))

-- | Transform a Dirent into a [Dirent], representing the entries at depth n from the provided dirent. Passing a
-- DirentFile into this function will result in []. A depth of zero means "this" level of depth.
direntsAtDepth :: Int -> Dirent -> [Dirent]
direntsAtDepth 0 d                         = [d]
direntsAtDepth _ d@(DirentFile _)          = [d]
direntsAtDepth n (DirentDir _ children) = concatMap (direntsAtDepth (n-1)) children
