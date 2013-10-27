module Main where

import System.Process (readProcess)

import qualified Data.Map as M

import Cli (Cli(..), OptVerbose, parseCli)
import Dirent (makeDirents)
import Sloch (PathToLangToSloc, sloch, slochDirents, summarize')
import Sloch.Show (showLangToSloc, showPathToLangToSloc)

main :: IO ()
main = parseCli >>= main'

main' :: Cli -> IO ()
main' cli = do
    let depth            = cliDepth cli
        include_dotfiles = cliIncludeDotfiles cli
        git              = cliGit cli
        verbose          = cliVerbose cli
        dir              = cliDir cli

    if git
        then gitSloch verbose
        else do
            s <- sloch depth include_dotfiles dir
            putStrLn $ showPathToLangToSloc verbose s

gitSloch :: OptVerbose -> IO ()
gitSloch verbose =
    readProcess "git" ["ls-files"] "" >>= \files ->
    makeDirents (lines files) True >>=
    slochDirents >>=
    putStr . showLangToSloc verbose . summarize'

slochFiles :: [FilePath] -> Bool -> IO PathToLangToSloc
slochFiles paths include_dotfiles = fmap M.unions $
    mapM (sloch 0 include_dotfiles) paths
