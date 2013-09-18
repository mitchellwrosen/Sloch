module Main where

import System.Environment (getArgs)

import Sloch (sloch)

main :: IO ()
main = do
    [dir, depth] <- getArgs
    s <- sloch dir (read depth)
    print s

