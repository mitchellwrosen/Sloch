{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}

module Dirent
    ( Dirent(..)
    , direntsAtDepth
    , makeDirent
    , makeDirents
    ) where

import Control.Applicative
import Control.Concurrent.Async (concurrently)
import Control.Exception        (IOException, catch)
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Maybe               (catMaybes)

import System.Directory.Extras   (getDirectoryContents)
import System.FilePath.Extras    (isDotfile)
import System.Posix.Files.Extras (hasReadPermission, isDirectory, isSymbolicLink)

data Dirent
    = DirentDir FilePath [Dirent]
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
makeDirent include_dotfiles path = catch (runMaybeT makeDirent') (\(_ :: IOException) -> return Nothing)
  where
    makeDirent' :: MaybeT IO Dirent
    makeDirent' = do
        guard  $ include_dotfiles || not (isDotfile path)
        guardM $ lift $ isNotSymbolicLink path
        guardM $ lift $ hasReadPermission path

        is_dir <- lift $ isDirectory path
        if is_dir
            then do
               children        <- lift $ getDirectoryContents path
               childrenDirents <- lift $ concurrently' $ map (makeDirent include_dotfiles) children
               return $ DirentDir path (catMaybes childrenDirents)
            else return $ DirentFile path
      where
        isNotSymbolicLink :: FilePath -> IO Bool
        isNotSymbolicLink = fmap not . isSymbolicLink

guardM :: MonadPlus m => m Bool -> m ()
guardM action = action >>= guard

concurrently' :: [IO a] -> IO [a]
concurrently' = foldr f (return [])
  where
    f :: IO a -> IO [a] -> IO [a]
    f ioa ioas = uncurry (:) <$> concurrently ioa ioas

makeDirents :: Bool -> [FilePath] -> IO [Dirent]
makeDirents include_dotfiles = fmap catMaybes . mapM (makeDirent include_dotfiles)

-- | Transform a Dirent into a [Dirent], representing the entries at depth n from the provided dirent. Passing a
-- DirentFile into this function will result in []. A depth of zero means "this" level of depth.
direntsAtDepth :: Int → Dirent → [Dirent]
direntsAtDepth 0 d                      = [d]
direntsAtDepth _ d@(DirentFile _)       = [d]
direntsAtDepth n (DirentDir _ children) = concatMap (direntsAtDepth (n-1)) children
