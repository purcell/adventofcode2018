module Day02
  ( main
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.List (tails)
import qualified Parse as P
import Parse (Parser)

ids :: Parser [String]
ids = P.many (P.many P.letterChar <* P.newline)

freqs :: String -> Map Char Int
freqs = Map.unionsWith (+) . fmap (`Map.singleton` 1)

inCommon :: Eq a => [a] -> [a] -> [a]
inCommon a b = [x | (x, y) <- zip a b, x == y]

oneDifferent :: Eq a => [a] -> [a] -> Bool
oneDifferent a b
  | length a /= length b = False
  | otherwise = length (inCommon a b) == length a - 1

distinctPairs :: [a] -> [(a, a)]
distinctPairs xs = [(x, y) | (x:xs') <- tails xs, y <- xs']

main :: IO ()
main = do
  ids <- P.parseFile ids "input/2.txt"
  putStrLn "Part 1:"
  let hasN n = filter (elem n . Map.elems) (freqs <$> ids)
  print (length (hasN 2) * length (hasN 3))
  putStrLn "Part 2:"
  print [inCommon a b | (a, b) <- distinctPairs ids, oneDifferent a b]
