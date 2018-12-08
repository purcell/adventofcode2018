{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Control.Arrow ((&&&))
import Data.Char (ord)
import Data.List ((\\), sortOn)
import Data.Maybe (fromMaybe)
import qualified Parse as P
import Safe (minimumMay)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as GPT

type StepGraph = GPT.Gr Char ()

data Step = Step
  { sName :: Char
  , sNode :: G.Node
  } deriving (Eq, Show)

toGraph :: [(Char, Char)] -> StepGraph
toGraph links = G.mkGraph nodes edges
  where
    nodes = (ord &&& id) <$> (fmap fst links ++ fmap snd links)
    edges = (\(a, b) -> (ord a, ord b, ())) <$> links

part2 :: Int -> Int -> StepGraph -> Int
part2 nworkers fixedDelay = go [] 0
  where
    go _ time gr
      | G.isEmpty gr = time
    go allocs time gr = go allocs' time' gr'
      where
        done = filter ((<= time) . snd) allocs
        gr' = G.delNodes (sNode . fst <$> done) gr
        continuingAllocs = allocs \\ done
        alreadyWorkingOn s = s `elem` fmap fst allocs
        nexts =
          take
            (nworkers - length continuingAllocs)
            (filter (not . alreadyWorkingOn) (avail gr'))
        allocs' = fmap (id &&& newTime) nexts ++ continuingAllocs
        newTime s = time + fixedDelay + (ord (sName s) - ord 'A' + 1)
        time' = fromMaybe time (minimumMay (snd <$> allocs'))

part1 :: StepGraph -> String
part1 gr =
  case avail gr of
    [] -> []
    (s:_) -> sName s : part1 (G.delNode (sNode s) gr)

avail :: StepGraph -> [Step]
avail g = sortOn sName $ toStep <$> G.nodes (G.nfilter (null . G.inn g) g)
  where
    toStep n = Step (G.lab' (G.context g n)) n

parseConstraint :: P.Parser (Char, Char)
parseConstraint =
  (,) <$> ("Step " *> P.letterChar) <*>
  (" must be finished before step " *> P.letterChar <* " can begin." <*
   P.newline)

example :: [(Char, Char)]
example =
  [ ('C', 'A')
  , ('C', 'F')
  , ('A', 'B')
  , ('A', 'D')
  , ('B', 'E')
  , ('D', 'E')
  , ('F', 'E')
  ]

main :: IO ()
main = do
  graph <-
    toGraph <$> P.parseFile (P.some parseConstraint <* P.eof) "input/7.txt"
  putStrLn "Part 1:"
  putStrLn (part1 graph)
  putStrLn "Part 2:"
  print (part2 5 60 graph)
