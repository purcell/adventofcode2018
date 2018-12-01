{-# LANGUAGE ScopedTypeVariables #-}

module Day01
  ( main
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

numbers :: P.Parsec Void Text [Int]
numbers = P.many (L.signed (pure ()) L.decimal <* P.newline)

nums :: IO [Int]
nums = do
  let f = "input/1.txt"
  result <- P.parse numbers f <$> TIO.readFile f
  case result of
    Left e -> error (show e)
    Right ns -> pure ns

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
