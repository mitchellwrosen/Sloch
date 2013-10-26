module Sloch.Show
    ( showLangToSloc
    , showPathToLangToSloc
    ) where

import Data.List (sort, sortBy)
import Data.Monoid ((<>))

import qualified Data.Map as M

import Cli (OptVerbose)
import Language (Language)
import Sloch (LangToSloc, PathToLangToSloc)

showLangToSloc :: OptVerbose -> LangToSloc -> String
showLangToSloc verbose = unlines . map display . sortBy compareSloc . M.toList
  where
    compareSloc :: (Language, [(FilePath, Int)]) -> (Language, [(FilePath, Int)]) -> Ordering
    compareSloc (lang1, fs1) (lang2, fs2) = sumSnds fs2 `compare` sumSnds fs1 <> lang1 `compare` lang2

    display :: (Language, [(FilePath, Int)]) -> String
    display (lang, fs) = unlines $
        show lang : lineCounts
      where
        lineCounts :: [String]
        lineCounts =
            if verbose
                then map (\(fp,n) -> "      " ++ show n ++ " " ++ fp) (sortBy (\a b -> snd b `compare` snd a) fs)
                else ["      " ++ show (sumSnds fs)]

sumSnds :: Num b => [(a,b)] -> b
sumSnds = foldr ((+) . snd) 0

showPathToLangToSloc :: OptVerbose -> PathToLangToSloc -> String
showPathToLangToSloc verbose = unlines . concatMap display . sort . M.toList
  where
    display :: (FilePath, LangToSloc) -> [String]
    display (path, s) = path : map ("   " ++) (lines $ showLangToSloc verbose s)
