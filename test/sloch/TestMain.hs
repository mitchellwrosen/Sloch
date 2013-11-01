module Main where

import Test.Tasty

import qualified DirentTest

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ DirentTest.tests
    ]
