module Sloch.Show
    ( showSloch
    , showSlochHierarchy
    ) where


import Text.PrettyPrint
import Data.List (sort, sortBy)
import Data.Monoid (mappend)

import qualified Data.Map as M

import Language (Language)
import Sloch.Types (Sloch, SlochHierarchy)

showSloch :: Sloch -> String
showSloch = render . slochDoc
  where
    slochDoc :: Sloch -> Doc
    slochDoc = slochDoc' . sortBy compareSloc . M.toList
      where
        compareSloc :: (Language, Int) -> (Language, Int) -> Ordering
        compareSloc (lang1, n1) (lang2, n2) = (n2 `compare` n1) `mappend` (lang1 `compare` lang2)

        slochDoc' :: [(Language, Int)] -> Doc
        slochDoc' xs = (vcat . map row) xs
          where
            row :: (Language, Int) -> Doc
            row (lang, n) = sizedText width (show lang) <+> int n

            width :: Int
            width = 10 + foldr (\s n -> max (length . show . fst $ s) n) 0 xs

showSlochHierarchy :: SlochHierarchy -> String
showSlochHierarchy = unlines . concatMap display . sort . M.toList
  where
    display :: (FilePath, Sloch) -> [String]
    display (path, s) = path : map ("   " ++) (lines $ showSloch s)
