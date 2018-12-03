module Util
  (distinctPairs)
  where

import Data.List (tails)

distinctPairs :: [a] -> [(a, a)]
distinctPairs xs = [(x, y) | (x:xs') <- tails xs, y <- xs']

