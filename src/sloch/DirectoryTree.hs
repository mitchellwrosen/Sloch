{-# LANGUAGE RankNTypes, TupleSections #-}

module DirectoryTree
    ( Dirent(..)
    , makeDirent
    , direntsAtDepth
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
direntsAtDepth 0 d                         = [d]
direntsAtDepth _ d@(DirentFile _)          = [d]
direntsAtDepth n (DirentDir (_, children)) = concatMap (direntsAtDepth (n-1)) children
