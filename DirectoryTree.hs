{-# LANGUAGE RankNTypes #-}

module DirectoryTree
    ( Directory
    , DirectoryTree
    , Dirent(..)
    , makeTree
    , treesAtDepth
    ) where

import Control.Applicative ((<$>))
import Control.Monad (filterM, forever)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Monoid ((<>))
import LineCounter (countLines)
import Pipes
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus, isDirectory, isSymbolicLink)

import qualified Pipes.Prelude as P
import qualified System.Directory as D

import Prelude hiding (appendFile)
import Debug.Trace

type DirectoryTree = Directory
type Directory = (FilePath, [Dirent])

data Dirent = DirentDir Directory
            | DirentFile FilePath
            deriving Show

-- | Build a DirectoryTree from the specified directory, excluding ".", "..", and symlinks from each directory.
-- Optionally include dotfiles.
makeTree :: FilePath -> Bool -> IO DirectoryTree
makeTree file_path include_dotfiles = foldM step begin contents
  where
    step :: DirectoryTree -> FilePath -> IO DirectoryTree
    step tree file_path =
        ifM (isDirectory <$> getFileStatus file_path)
            (appendDir tree file_path)
            (return $ appendChild tree $ DirentFile file_path)

    appendDir :: Directory -> FilePath -> IO Directory
    appendDir dir file_path = do
        child <- makeTree file_path include_dotfiles
        return $ appendChild dir (DirentDir child)

    begin :: IO DirectoryTree
    begin = return (file_path, [])

    contents :: Producer FilePath IO ()
    contents = getDirectoryContents file_path include_dotfiles

-- | Like Pipes.Prelude.foldM, but no explicit end step (simply return).
foldM :: Monad m => (b -> a -> m b) -> m b -> Producer a m () -> m b
foldM step begin = P.foldM step begin return

-- | Enumerate the specified directory, excluding ".", "..", and symlinks. Optionally include dotfiles.
getDirectoryContents :: FilePath -> Bool -> Producer' FilePath IO ()
getDirectoryContents file_path include_dotfiles = lift contents >>= each
  where
    -- This order is important. Filter "." and ".." before prepending the directory name, which is necessary before
    -- checking if the file is a symlink (need entire path, of course).
    contents :: IO [FilePath]
    contents = filteredAbsolutePaths >>= filterSymlinks

    -- Create absolute paths after ".", "..", and possibly dotfiles have been filtered.
    filteredAbsolutePaths :: IO [FilePath]
    filteredAbsolutePaths =
        map (file_path </>) . possiblyFilterDotfiles . filter (`notElem` [".",".."]) <$>
            D.getDirectoryContents file_path

    possiblyFilterDotfiles :: [FilePath] -> [FilePath]
    possiblyFilterDotfiles xs = let f = if include_dotfiles then id else filter (not . isPrefixOf ".") in f xs

    filterSymlinks :: [FilePath] -> IO [FilePath]
    filterSymlinks = filterM $ fmap not . isSymbolicLink'

    isSymbolicLink' :: FilePath -> IO Bool
    isSymbolicLink' = fmap isSymbolicLink . getSymbolicLinkStatus

appendChild :: Directory -> Dirent -> Directory
appendChild (fp, xs) t = (fp, t:xs)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a1 a2 = mb >>= \b -> if b then a1 else a2

-- | Transform a DirectoryTree into a [DirectoryTree], representing the trees at depth n from the root tree. A depth of
-- zero will result in a singleton list containing the root tree.
treesAtDepth :: Int -> DirectoryTree -> [DirectoryTree]
treesAtDepth 0 t = [t]
treesAtDepth n (_, dirents) = concatMap (treesAtDepth' (n-1)) dirents
  where
    treesAtDepth' :: Int -> Dirent -> [DirectoryTree]
    treesAtDepth' 0 (DirentDir (path, cs)) = [(path, cs)]
    treesAtDepth' 0 (DirentFile path) = []
    treesAtDepth' n (DirentDir (_, cs)) = concatMap (treesAtDepth' (n-1)) cs
    treesAtDepth' n (DirentFile path) = []
