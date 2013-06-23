module MapUtils where

import qualified Data.Map as Map

-- adjustWithDefault def f key m
--
-- Adjusts map |m| at key |key|, if it exists, with |f|. Otherwise, insert
-- |def| into |m| with key |key|.
adjustWithDefault :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
adjustWithDefault def f key m =
   if key `Map.member` m
   then Map.adjust f key m
   else Map.insert key def m
