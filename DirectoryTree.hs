{-# LANGUAGE RankNTypes #-}

module DirectoryTree
    ( Directory
    , DirectoryTree
    , Dirent(..)
    , makeTree
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Map (Map)
import Data.Monoid ((<>))
import LineCounter (countLines)
import Pipes
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isDirectory)

import qualified Pipes.Prelude as P
import qualified System.Directory as D

import Prelude hiding (appendFile)

type DirectoryTree = Directory
type Directory = (FilePath, [Dirent])

data Dirent = DirentDir Directory
            | DirentFile FilePath
            deriving Show

-- | Build a DirectoryTree from the specified directory, excluding "." and ".." from each directory.
makeTree :: FilePath -> IO DirectoryTree
makeTree file_path = foldM step begin contents
  where
    step :: DirectoryTree -> FilePath -> IO DirectoryTree
    step tree file_path =
        ifM (isDirectory <$> getFileStatus file_path)
            (appendDir tree file_path)
            (return $ appendChild tree $ DirentFile file_path)

    appendDir :: Directory -> FilePath -> IO Directory
    appendDir dir file_path = do
        child <- makeTree file_path
        return $ appendChild dir (DirentDir child)

    begin :: IO DirectoryTree
    begin = return (file_path, [])

    contents :: Producer FilePath IO ()
    contents = getDirectoryContents file_path >-> P.map (file_path </>)

-- Like Pipes.Prelude.foldM, but no explicit end step (simply return).
foldM :: Monad m => (b -> a -> m b) -> m b -> Producer a m () -> m b
foldM step begin = P.foldM step begin return

-- | Enumerate the specified directory, excluding "." and ".."
getDirectoryContents :: FilePath -> Producer' FilePath IO ()
getDirectoryContents file_path = lift contents >>= each
  where
    contents :: IO [FilePath]
    contents = filterThisAndParent <$> D.getDirectoryContents file_path

    filterThisAndParent :: [FilePath] -> [FilePath]
    filterThisAndParent = filter (`notElem` [".", ".."])

appendChild :: Directory -> Dirent -> Directory
appendChild (fp, xs) t = (fp, (t:xs))

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a1 a2 = mb >>= \b -> if b then a1 else a2
