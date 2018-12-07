{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Control.Arrow ((&&&), second)
import Data.Foldable (all, minimum, minimumBy)
import Data.Function (on)
import qualified Data.Ix
import qualified Data.Ix as Ix
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

part1 :: [Coord] -> Int
part1 = maximum . fmap length . Map.elems . closests

part2 :: Int -> [Coord] -> Int
part2 limit coords =
  length
    [c | c <- Ix.range (bounds coords), sum (manhattan c <$> coords) < limit]

type Region = (Coord, Coord)

bounds :: [Coord] -> Region
bounds coords = (V2 minX minY, V2 maxX maxY)
  where
    xs = [x | (V2 x _) <- coords]
    ys = [y | (V2 x y) <- coords]
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

isOnEdge :: Region -> Coord -> Bool
isOnEdge (V2 minX minY, V2 maxX maxY) (V2 x y) =
  x == minX || x == maxX || y == minY || y == maxY

closests :: [Coord] -> Map Coord [Coord]
closests coords = Map.filter (all (not . isOnEdge region)) distsMap
  where
    region = bounds coords
    distsMap = Map.fromListWith (++) (second (: []) <$> dists)
    dists =
      [ (to, from)
      | from <- Ix.range region
      , let ((closestDist, to):rest) =
              sortOn fst $ (manhattan from &&& id) <$> coords
      , closestDist `notElem` (fst <$> rest)
      ]

example :: [Coord]
example = [V2 1 1, V2 1 6, V2 8 3, V2 3 4, V2 5 5, V2 8 9]

main :: IO ()
main = do
  coords <- P.parseFile (parseCoords <* P.eof) "input/6.txt"
  putStrLn "Part 1:"
  print (part1 coords)
  putStrLn "Part 2:"
  print (part2 10000 coords)
