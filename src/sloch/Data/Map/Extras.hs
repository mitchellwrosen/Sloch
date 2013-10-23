module Data.Map.Extras where

import Data.Map (Map, adjust, insert, member)

adjustWithDefault :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustWithDefault f k a m = if member k m then adjust f k m else insert k a m
