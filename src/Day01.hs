{-# LANGUAGE ScopedTypeVariables #-}
module Day01
    ( main
    ) where

readInt :: String -> Int
readInt ('+':s) = read s
readInt s = read s

main :: IO ()
main = do
  nums :: [Int] <- fmap readInt . lines <$> readFile "input/1.txt"
  print $ sum nums

