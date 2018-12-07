{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parse as P
import Parse (Parser)
import qualified Util

data Claim = Claim
  { cID :: Int
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
  Set.fromList
    [ (left, top)
    | left <- [max (rLeft r1) (rLeft r2) .. min (right r1) (right r2)]
    , top <- [max (rTop r1) (rTop r2) .. min (bottom r1) (bottom r2)]
    ]
  where
    right r = rLeft r + rWidth r - 1
    bottom r = rTop r + rHeight r - 1

parseClaim :: Parser Claim
parseClaim =
  Claim <$> ("#" *> P.decimal <* " @ ") <*>
  (Rect <$> (P.decimal <* ",") <*> (P.decimal <* ": ") <*> (P.decimal <* "x") <*>
   P.decimal)

parseClaims :: Parser [Claim]
parseClaims = P.many (parseClaim <* P.newline)

examples :: [Claim]
examples =
  P.unsafeParseString
    parseClaims
    "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"

part1 :: [Claim] -> Int
part1 claims =
  Set.size $
  Set.unions [overlap (cRect a) (cRect b) | (a, b) <- Util.distinctPairs claims]

part2 :: [Claim] -> Set Int
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
