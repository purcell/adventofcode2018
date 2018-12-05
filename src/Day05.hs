module Day05 where

import Data.Char (toLower, toUpper)
import qualified Parse as P

shouldReduce :: Char -> Char -> Bool
shouldReduce a b = a /= b && (a == toLower b || a == toUpper b)

reducePolymer :: String -> String
reducePolymer = foldr squish ""
  where
    squish a (b:xs)
      | shouldReduce a b = xs
      | otherwise = a:b:xs
    squish a xs = a:xs

strip :: String -> Char -> String
strip s c = filter (\x -> x /= toLower c && x /= toUpper c) s

main :: IO ()
main = do
  polymer <- P.parseFile (P.some P.letterChar :: P.Parser String) "input/5.txt"
  putStrLn "Part 1:"
  print (length (reducePolymer polymer))
  putStrLn "Part 2:"
  print (minimum (length . reducePolymer . strip polymer <$> ['a' .. 'z']))
