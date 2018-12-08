{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Control.Monad (replicateM)
import qualified Data.Tree as Tree
import qualified Parse as P

parseTree :: P.Parser (Tree.Tree [Int])
parseTree = do
  childCount <- P.space *> P.decimal
  valueCount <- P.space *> P.decimal
  children <- replicateM childCount (P.space *> parseTree)
  values <- replicateM valueCount (P.space *> P.decimal)
  pure $ Tree.Node values children

part1 :: Tree.Tree [Int] -> Int
part1 = sum . concat . Tree.flatten

part2 :: Tree.Tree [Int] -> Int
part2 (Tree.Node vals []) = sum vals
part2 (Tree.Node indexes children) = sum (childValueAt <$> indexes)
  where
    indexed = zip [1 ..] children
    childValueAt n = maybe 0 part2 (lookup n indexed)

example :: Tree.Tree [Int]
example = P.unsafeParseString parseTree "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main :: IO ()
main = do
  tree <- P.parseFile parseTree "input/8.txt"
  putStrLn "Part 1:"
  print (part1 tree)
  putStrLn "Part 2:"
  print (part2 tree)
