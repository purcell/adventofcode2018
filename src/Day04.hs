{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day04 where

import Control.Applicative
import Data.List (maximumBy, sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Parse as P
import Parse (Parser)
import Safe.Foldable (maximumByMay)
import qualified Util

newtype Minute =
  Minute Int
  deriving (Eq, Ord, Enum, Num)

newtype GuardNum =
  GuardNum Int
  deriving (Eq, Ord)

data Nap =
  Nap GuardNum
      [Minute]

parseNaps :: Parser [Nap]
parseNaps = P.some (Nap <$> guardNum <*> (concat <$> P.many (P.try nap)))
  where
    event :: Parser a -> Parser (Minute, a)
    event p = (,) <$> timestamp <*> (p <* P.newline)
    guardNum = snd <$> event beginShift
    beginShift = "Guard #" *> (GuardNum <$> int) <* " begins shift"
    nap = do
      (sleep, _) <- event "falls asleep"
      (wake, _) <- event "wakes up"
      pure [sleep .. wake - 1]
    date = P.some (P.digitChar <|> P.char '-')
    time = int >> ":" >> (Minute <$> int)
    int = P.decimal :: Parser Int
    timestamp = "[" >> date >> P.space *> time <* "] "

minsAsleep :: [Nap] -> [(GuardNum, Map Minute Int)]
minsAsleep =
  Map.toList .
  foldl
    (\m (Nap num mins) ->
       Map.insertWith (Map.unionWith (+)) num (Util.freqs mins) m)
    Map.empty

part1 :: [Nap] -> Int
part1 naps = mostSleptMinute * guardNum
  where
    sleepFreqs = minsAsleep naps
    (Minute mostSleptMinute) =
      fst (maximumBy (comparing snd) (Map.toList sleepMins))
    (GuardNum guardNum, sleepMins) =
      maximumBy (comparing (sum . Map.elems . snd)) sleepFreqs

part2 :: [Nap] -> Int
part2 naps = mostSleptMinute * guardNum
  where
    sleepFreqs = minsAsleep naps
    guardsAndSleepiestMinutes :: [(GuardNum, (Minute, Int))]
    guardsAndSleepiestMinutes =
      [ (guard, most)
      | (guard, freqs) <- sleepFreqs
      , Just most <- [maximumByMay (comparing snd) (Map.toList freqs)]
      ]
    (GuardNum guardNum, (Minute mostSleptMinute, _)) =
      maximumBy (comparing (snd . snd)) guardsAndSleepiestMinutes

main :: IO ()
main = do
  naps <-
    P.unsafeParseString parseNaps . unlines . sort . lines <$>
    readFile "input/4.txt"
  putStrLn "Part 1:"
  print $ part1 naps
  putStrLn "Part 2:"
  print $ part2 naps
