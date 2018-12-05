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
    beginShift =
      P.string "Guard #" *> (GuardNum <$> P.decimal) <* P.string " begins shift"
    nap = do
      (sleep, _) <- event (P.string "falls asleep")
      (wake, _) <- event (P.string "wakes up")
      pure [sleep .. wake - 1]
    date = P.some (P.digitChar <|> P.char '-')
    time = P.decimal >> P.char ':' >> (Minute <$> P.decimal)
    timestamp = P.char '[' >> date >> P.space *> time <* P.string "] "

minsAsleep :: [Nap] -> Map GuardNum [Minute]
minsAsleep =
  foldl (\map (Nap num mins) -> Map.insertWith (++) num mins map) Map.empty

part1 :: [Nap] -> Int
part1 naps = mostSleptMinute * guardNum
  where
    (Minute mostSleptMinute) =
      fst $ maximumBy (comparing snd) $ Map.toList $ Util.freqs sleepMins
    (GuardNum guardNum, sleepMins) =
      maximumBy (comparing (length . snd)) $ Map.toList $ minsAsleep naps

main :: IO ()
main = do
  naps <-
    P.unsafeParseString parseNaps . unlines . sort . lines <$>
    readFile "input/4.txt"
  putStrLn "Part 1:"
  print $ part1 naps
