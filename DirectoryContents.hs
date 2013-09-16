{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Foo
    ( getDirectoryContents
    , getRecursiveDirectoryContents
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Monoid ((<>))
import LineCounter (countLines)
import Pipes
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isDirectory)

import qualified Pipes.Prelude as P
import qualified System.Directory as D

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a1 a2 = mb >>= \b -> if b then a1 else a2

getDirectoryContents :: FilePath -> Producer' FilePath IO ()
getDirectoryContents file_path = lift contents >>= each
  where
    contents :: IO [FilePath]
    contents = filterThisAndParent <$> D.getDirectoryContents file_path

    filterThisAndParent :: [FilePath] -> [FilePath]
    filterThisAndParent = filter (`notElem` [".", ".."])

getRecursiveDirectoryContents :: FilePath -> Producer' FilePath IO ()
getRecursiveDirectoryContents file_path =
    getDirectoryContents file_path >->
    P.map (file_path </>)          >->
    recurse
  where
    recurse :: Pipe FilePath FilePath IO ()
    recurse = forever $ do
        file_path <- await
        ifM (isDirectory <$> lift (getFileStatus file_path))
            (getRecursiveDirectoryContents file_path)
            (yield file_path)
