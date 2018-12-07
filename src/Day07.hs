{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Control.Arrow ((&&&))
import Data.Char (ord)
import Data.List (sortOn)
import qualified Parse as P

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as GPT

type StepGraph = GPT.Gr Char ()

toGraph :: [(Char, Char)] -> StepGraph
toGraph links = G.mkGraph nodes edges
  where
    nodes :: [(Int, Char)]
    nodes = (ord &&& id) <$> (fmap fst links ++ fmap snd links)
    edges = (\(a, b) -> (ord a, ord b, ())) <$> links

stepOrder :: [(Char, Char)] -> String
stepOrder = go . toGraph
  where
    go gr =
      case sortOn G.lab' (avail gr) of
        [] -> []
        (ctx:_) -> G.lab' ctx : go (G.delNode (G.node' ctx) gr)
    avail g = G.context g <$> G.nodes (G.nfilter (null . G.inn g) g)

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
  constraints <- P.parseFile (P.some parseConstraint <* P.eof) "input/7.txt"
  putStrLn "Part 1:"
  putStrLn (stepOrder constraints)
