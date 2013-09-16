{-# LANGUAGE LambdaCase, RankNTypes #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Map (Map)
import Pipes
import Pipes.Lift (execStateP)
import Control.Monad.Trans.State.Strict (StateT, modify)

import qualified Data.Map      as M

import DirectoryContents (getRecursiveDirectoryContents)
import Language (language)
import LineCounter (countLines)

main :: IO ()
main = do
    counts <-
        runEffect $
            execStateP M.empty $
                hoist lift (getRecursiveDirectoryContents ".") >-> sloch
    print counts

-- | Consume FilePaths, building up a Map from FilePath to Int, which is each file's source line count.
sloch :: Consumer' FilePath (StateT (Map FilePath Int) IO) ()
sloch = forever $ do
    file_path <- await
    whenMaybe (language file_path) $ \lang -> do
        n <- liftIO $ countLines file_path lang
        lift $ modify (M.insert file_path n)

-- | Like when, but conditional on a Maybe being Just rather than a Bool.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()
