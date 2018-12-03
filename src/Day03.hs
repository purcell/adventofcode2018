{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import Data.List (tails)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parse as P
import Parse (Parser)

data Claim = Claim
  { cID :: String
  , cRect :: Rect
  }

data Rect = Rect
  { rLeft :: Int
  , rTop :: Int
  , rWidth :: Int
  , rHeight :: Int
  }

overlap :: Rect -> Rect -> Set (Int, Int)
overlap r1 r2 =
  Set.fromList
    [ (left, top)
    | left <- [rLeft r1 .. rLeft r1 + rWidth r1 - 1]
    , top <- [rTop r1 .. rTop r1 + rHeight r1 - 1]
    , left >= rLeft r2
    , left < rLeft r2 + rWidth r2
    , top >= rTop r2
    , top < rTop r2 + rHeight r2
    ]

parseClaim :: Parser Claim
parseClaim = do
  P.char '#'
  cID <- P.many P.digitChar
  P.space
  P.char '@'
  P.space
  rLeft <- P.decimal
  P.char ','
  rTop <- P.decimal
  P.char ':'
  P.space
  rWidth <- P.decimal
  P.char 'x'
  rHeight <- P.decimal
  let cRect = Rect {..}
  pure Claim {..}

parseClaims :: Parser [Claim]
parseClaims = P.many (parseClaim <* P.newline)

distinctPairs :: [a] -> [(a, a)]
distinctPairs xs = [(x, y) | (x:xs') <- tails xs, y <- xs']

examples =
  P.parse parseClaims "string" "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"

part1 :: [Claim] -> Int
part1 claims =
  Set.size $
  Set.unions [overlap (cRect a) (cRect b) | (a, b) <- distinctPairs claims]

main :: IO ()
main = do
  claims <- P.parseFile parseClaims "input/3.txt"
  putStrLn "Part 1:"
  print (part1 claims)
