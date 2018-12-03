{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parse as P
import Parse (Parser)
import qualified Util

data Claim = Claim
  { cID :: String
  , cRect :: Rect
  }

data Rect = Rect
  { rLeft :: !Int
  , rTop :: !Int
  , rWidth :: !Int
  , rHeight :: !Int
  }

overlap :: Rect -> Rect -> Set (Int, Int)
overlap r1 r2 =
  let right r = rLeft r + rWidth r - 1
      bottom r = rTop r + rHeight r - 1
   in Set.fromList
        [ (left, top)
        | left <- [max (rLeft r1) (rLeft r2) .. min (right r1) (right r2)]
        , top <- [max (rTop r1) (rTop r2) .. min (bottom r1) (bottom r2)]
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

examples =
  P.unsafeParseString
    parseClaims
    "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"

part1 :: [Claim] -> Int
part1 claims =
  Set.size $
  Set.unions [overlap (cRect a) (cRect b) | (a, b) <- Util.distinctPairs claims]

part2 :: [Claim] -> Set String
part2 claims = Set.fromList (cID <$> claims) `Set.difference` overlapping
  where
    overlapping =
      Set.unions
        [ Set.fromList [cID a, cID b]
        | (a, b) <- Util.distinctPairs claims
        , not (Set.null (overlap (cRect a) (cRect b)))
        ]

main :: IO ()
main = do
  claims <- P.parseFile parseClaims "input/3.txt"
  putStrLn "Part 1:"
  print (part1 claims)
  putStrLn "Part 2:"
  print (part2 claims)
