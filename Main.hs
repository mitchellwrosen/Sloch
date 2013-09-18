module Main where

import System.Environment (getArgs)

import Options (Options(..), parseOptions)
import Sloch (slochHierarchy)

main :: IO ()
main = do
    (opts, [dir]) <- parseOptions =<< getArgs

    let depth            = optDepth opts
        include_dotfiles = optIncludeDotfiles opts

    s <- slochHierarchy dir depth include_dotfiles
    print s
