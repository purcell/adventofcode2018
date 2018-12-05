module Util
  ( distinctPairs
  , freqs
  ) where

import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as Map

distinctPairs :: [a] -> [(a, a)]
distinctPairs xs = [(x, y) | (x:xs') <- tails xs, y <- xs']

freqs :: Ord a => [a] -> Map a Int
freqs = Map.unionsWith (+) . fmap (`Map.singleton` 1)
