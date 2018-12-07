module Day01
  ( main
  ) where

import qualified Data.Set as Set

import qualified Parse as P
import Parse (Parser)

numbers :: Parser [Int]
numbers = P.many (P.signedInt <* P.newline)

main :: IO ()
main = do
  ns <- P.parseFile numbers "input/1.txt"
  putStrLn "Part 1:"
  print (sum ns)
  putStrLn "Part 2:"
  print (firstDup (scanl (+) 0 (cycle ns)))

firstDup :: (Eq a, Ord a) => [a] -> Maybe a
firstDup xs = go xs Set.empty
  where
    go [] _ = Nothing
    go (y:ys) seen
      | y `Set.member` seen = Just y
      | otherwise = go ys (Set.insert y seen)
