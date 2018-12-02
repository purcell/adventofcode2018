module Day02
  ( main
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

parseFile :: Show e => P.Parsec e Text a -> String -> IO a
parseFile p f = do
  result <- P.parse p f <$> TIO.readFile f
  case result of
    Left e -> error (show e)
    Right res -> pure res

ids :: P.Parsec Void Text [String]
ids = P.many (P.many P.letterChar <* P.newline)

freqs :: String -> Map Char Int
freqs = Map.unionsWith (+) . fmap (flip Map.singleton 1)

main :: IO ()
main = do
  ids <- parseFile ids "input/2.txt"
  putStrLn "Part 1:"
  let hasN n = filter (elem n . Map.elems) (freqs <$> ids)
  print (length (hasN 2) * length (hasN 3))
