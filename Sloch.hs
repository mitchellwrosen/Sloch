{-# LANGUAGE LambdaCase, RankNTypes #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.Trans.State.Strict (StateT, modify, execStateT)
import Data.Map (Map)
import Pipes
import Pipes.Lift (execStateP)
import System.Environment (getArgs)

import qualified Data.Map      as M

import DirectoryTree (Dirent(..), DirectoryTree, makeTree)
import Language (Language, language)
import LineCounter (countLines)

type Sloch = Map Language Int

main :: IO ()
main = do
    [dir] <- getArgs
    tree <- makeTree dir
    s <- sloch tree
    print s

sloch :: DirectoryTree -> IO Sloch
sloch = flip execStateT M.empty . slochDirectory

slochDirectory :: DirectoryTree -> StateT Sloch IO ()
slochDirectory (file_path, children) = mapM_ slochDirent children

slochDirent :: Dirent -> StateT Sloch IO ()
slochDirent (DirentDir dir) = slochDirectory dir
slochDirent (DirentFile file_path) =
    whenMaybe (language file_path) $ \lang -> do
        n <- lift $ countLines file_path lang
        modify (M.insertWith (+) lang n)

-- | Like when, but conditional on a Maybe being Just rather than a Bool.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()
