{-# LANGUAGE ScopedTypeVariables #-}
module Day01
    ( main
    ) where

import qualified Data.Set as Set
import Data.Set (Set)

readInt :: String -> Int
readInt ('+':s) = read s
readInt s = read s

nums :: IO [Int]
nums = fmap readInt . lines <$> readFile "input/1.txt"

main :: IO ()
main = do
  ns <- nums
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
