module Sloch.Types 
    ( Sloch
    , SlochHierarchy
    ) where

import Data.Map (Map)

import Language (Language)

-- A simple summary of the source-lines-of-code count, by language.
type Sloch = Map Language [(FilePath, Int)]

-- A summary of the source-lines-of-code count, represented as two maps: the outer, a directory name to counts, and the
-- inner, a map from language type to number of lines.
type SlochHierarchy = Map FilePath Sloch
