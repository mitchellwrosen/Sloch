{-# LANGUAGE RankNTypes #-}

module DirectoryTree
    ( Directory
    , DirectoryTree
    , Dirent(..)
    , makeTree
    , treesAtDepth
    ) where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.Extras (ifM)
import Data.List (isPrefixOf)
import Pipes
import System.FilePath ((</>))
import System.FilePath.Extras (isDotfile)
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus, isDirectory, isSymbolicLink)

import Prelude hiding (appendFile)

import qualified Pipes.Prelude as P
import qualified System.Directory as D

type DirectoryTree = Directory
type Directory = (FilePath, [Dirent])
type File      = FilePath

data Dirent = DirentDir Directory
            | DirentFile File
            deriving Show

-- | Build a Dirent structure from the specified FilePath, which may be a file or directory. Ignores symlinks, ".",
-- "..", and optionally include dotfiles.
makeDirent :: FilePath -> Bool -> IO (Maybe Dirent)
makeDirent path include_dotfiles = do
    ifM (isDirectory <$> getFileStatus path)
        makeDirentDirectory
        makeDirentFile
  where
    makeDirentDirectory :: IO [Dirent]
    makeDirentDirectory = do
        contents <- map (path </>) . possiblyFilterDotfiles .
                            filter (`notElem` [".",".."]) <$> D.getDirectoryContents path
        contents' <- mapM (flip makeDirent include_dotfiles) contents
        return $ DirentDir (path, contents')

    makeDirentFile :: IO (Maybe Dirent)
    makeDirentFile =
        if include_dotfiles && isDotfile path
            then return DirentIgnored
            else return $ DirentFile path
    {-filteredAbsolutePaths :: IO [FilePath]-}
    {-filteredAbsolutePaths =-}
        {-map (file_path </>) . possiblyFilterDotfiles . filter (`notElem` [".",".."]) <$>-}
            {-D.getDirectoryContents file_path-}

    possiblyFilterDotfiles :: [FilePath] -> [FilePath]
    possiblyFilterDotfiles =
        (\paths -> if include_dotfiles then paths else filter (not . isPrefixOf ".") paths)

filterSymlinks :: [FilePath] -> IO [FilePath]
filterSymlinks = filterM $ fmap not . isSymbolicLink'

isSymbolicLink' :: FilePath -> IO Bool
isSymbolicLink' = fmap isSymbolicLink . getSymbolicLinkStatus

-- | Build a DirectoryTree from the specified directory, excluding ".", "..", and symlinks from each directory.
-- Optionally include dotfiles.
makeTree :: FilePath -> Bool -> IO DirectoryTree
makeTree file_path include_dotfiles = foldM step begin contents
  where
    step :: DirectoryTree -> FilePath -> IO DirectoryTree
    step tree path =
        ifM (isDirectory <$> getFileStatus path)
            (appendDir tree path)
            (return $ appendChild tree $ DirentFile path)

    appendDir :: Directory -> FilePath -> IO Directory
    appendDir dir path = do
        child <- makeTree path include_dotfiles
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

-- | Transform a DirectoryTree into a [DirectoryTree], representing the trees at depth n from the root tree. A depth of
-- zero will result in a singleton list containing the root tree.
treesAtDepth :: Int -> DirectoryTree -> [DirectoryTree]
treesAtDepth 0 t = [t]
treesAtDepth n (_, dirents) = concatMap (treesAtDepth' (n-1)) dirents
  where
    treesAtDepth' :: Int -> Dirent -> [DirectoryTree]
    treesAtDepth' 0 (DirentDir (path, cs)) = [(path, cs)]
    treesAtDepth' 0 (DirentFile _) = []
    treesAtDepth' m (DirentDir (_, cs)) = concatMap (treesAtDepth' (m-1)) cs
    treesAtDepth' _ (DirentFile _) = []
