{-# LANGUAGE RankNTypes, TupleSections #-}

module DirectoryTree
    ( DirectoryTree
    , Dirent(..)
    , makeTree
    , treesAtDepth
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (filterM)
import Control.Monad.Extras (ifM)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Pipes
import System.FilePath ((</>))

import Prelude hiding (appendFile)

import qualified Pipes.Prelude as P
import qualified System.Directory as D

import System.Directory.Extras (getDirectoryContents)
import System.FilePath.Extras (isDotfile)
import System.Posix.Files.Extras (isDirectory, isSymbolicLink)

-- TODO: Delete this. Dirent replaces DirectoryTree.
type DirectoryTree = (FilePath, [Dirent])

data Dirent = DirentDir (FilePath, [Dirent]) -- Path, plus children
            | DirentFile FilePath
            deriving Show

-- | Build a Maybe Dirent from the specified FilePath. Returns Nothing if the specified file should be ignored. A file
-- should be ignored if:
--      - it's a symlink
--      - it's a dotfile, and @ignore_dotfiles@ is True.
--      - the file cannot be opened or does not exist.
-- Otherwise, return a Just DirentDir (recursively create Dirents from each entry, ignoring "." and "..") or a Just
-- DirentFile.
makeDirent :: FilePath -> Bool -> IO (Maybe Dirent)
makeDirent path include_dotfiles = do
    ifM (isDirectory path)
        (Just <$> makeDirentDirectory)
        makeDirentFile
  where
    makeDirentDirectory :: IO Dirent
    makeDirentDirectory = DirentDir . (path, ) <$> makeDirentsFromChildren

    makeDirentsFromChildren :: IO [Dirent]
    makeDirentsFromChildren =
        catMaybes <$> (getDirectoryContents path >>= mapM (flip makeDirent include_dotfiles))

    makeDirentFile :: IO (Maybe Dirent)
    makeDirentFile = do
        is_symlink <- isSymbolicLink path
        return $
            if is_symlink || (include_dotfiles && isDotfile path)
                then Nothing
                else Just (DirentFile path)

-- | Transform a Dirent into a [Dirent], representing the entries at depth n from the provided dirent. Passing a
-- DirentFile into this function will result in []. A depth of zero means "this" level of depth.
direntsAtDepth :: Int -> Dirent -> [Dirent]
direntsAtDepth _ d@(DirentFile _)          = []
direntsAtDepth 0 dirent                    = [dirent]
direntsAtDepth n (DirentDir (_, children)) = concatMap (direntsAtDepth (n-1)) children

filterSymlinks :: [FilePath] -> IO [FilePath]
filterSymlinks = filterM $ fmap not . isSymbolicLink

-- | Build a DirectoryTree from the specified directory, excluding ".", "..", and symlinks from each directory.
-- Optionally include dotfiles.
-- TODO: Delete this function. makeDirent replaces it.
makeTree :: FilePath -> Bool -> IO DirectoryTree
makeTree file_path include_dotfiles = foldM step begin contents
  where
    step :: DirectoryTree -> FilePath -> IO DirectoryTree
    step tree path =
        ifM (isDirectory path)
            (appendDir tree path)
            (return $ appendChild tree $ DirentFile path)

    appendDir :: DirectoryTree -> FilePath -> IO DirectoryTree
    appendDir dir path = do
        child <- makeTree path include_dotfiles
        return $ appendChild dir (DirentDir child)

    begin :: IO DirectoryTree
    begin = return (file_path, [])

    contents :: Producer FilePath IO ()
    contents = getDirectoryContents' file_path include_dotfiles

-- | Like Pipes.Prelude.foldM, but no explicit end step (simply return).
foldM :: Monad m => (b -> a -> m b) -> m b -> Producer a m () -> m b
foldM step begin = P.foldM step begin return

-- | Enumerate the specified directory, excluding ".", "..", and symlinks. Optionally include dotfiles.
getDirectoryContents' :: FilePath -> Bool -> Producer' FilePath IO ()
getDirectoryContents' file_path include_dotfiles = lift contents >>= each
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
    filterSymlinks = filterM $ fmap not . isSymbolicLink

appendChild :: DirectoryTree -> Dirent -> DirectoryTree
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
