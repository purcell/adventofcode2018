{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Control.Arrow ((&&&), second)
import Data.Foldable (all, minimum, minimumBy)
import Data.Function (on)
import Data.List (groupBy, maximum, minimum, nub, sort, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Linear.V2
import qualified Parse as P

type Coord = V2 Int

parseCoords :: P.Parser [Coord]
parseCoords = P.some $ V2 <$> P.decimal <* ", " <*> P.decimal <* P.newline

manhattan :: Coord -> Coord -> Int
manhattan a b = abs dx + abs dy
  where
    (V2 dx dy) = a - b

bestAreaSize :: [Coord] -> Int
bestAreaSize = maximum . fmap length . Map.elems . closests

closests :: [Coord] -> Map Coord [Coord]
closests coords = Map.filter (all (not . isOnEdge)) distsMap
  where
    distsMap = Map.fromListWith (++) (second (: []) <$> dists)
    dists =
      [ (to, from)
      | from <- V2 <$> [minX .. maxX] <*> [minY .. maxY]
      , let ((closestDist, to):rest) =
              sortOn fst $ (manhattan from &&& id) <$> coords
      , closestDist `notElem` (fst <$> rest)
      ]
    xs = [x | (V2 x _) <- coords]
    ys = [y | (V2 x y) <- coords]
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    isOnEdge (V2 x y) = x == minX || x == maxX || y == minY || y == maxY

example :: [Coord]
example = [V2 1 1, V2 1 6, V2 8 3, V2 3 4, V2 5 5, V2 8 9]

main :: IO ()
main = do
  coords <- P.parseFile (parseCoords <* P.eof) "input/6.txt"
  putStrLn "Part 1:"
  print (bestAreaSize coords)
