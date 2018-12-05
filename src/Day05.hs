module Day05 where

import Data.Char (toLower, toUpper)
import qualified Parse as P

shouldReduce :: Char -> Char -> Bool
shouldReduce a b = a /= b && (a == toLower b || a == toUpper b)

reducePolymer :: String -> String
reducePolymer = go ""
  where
    go (p:ps) (n:ns)
      | shouldReduce p n = go ps ns
    go ps (n:n':ns)
      | shouldReduce n n' = go ps ns
    go ps (n:ns) = go (n : ps) ns
    go ps [] = reverse ps

strip :: String -> Char -> String
strip s c = filter (\x -> x /= c && (not ( shouldReduce x c))) s

main :: IO ()
main = do
  polymer <- P.parseFile (P.some P.letterChar :: P.Parser String) "input/5.txt"
  putStrLn "Part 1:"
  print (length (reducePolymer polymer))
  putStrLn "Part 2:"
  print (minimum (length . reducePolymer . strip polymer <$> ['a' .. 'z']))
