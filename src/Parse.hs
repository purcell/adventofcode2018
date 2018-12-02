module Parse
  ( module P
  , parseFile
  , Parser
  , signedInt
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = P.Parsec Void Text a

parseFile :: Show e => P.Parsec e Text a -> String -> IO a
parseFile p f = do
  result <- P.parse p f <$> TIO.readFile f
  case result of
    Left e -> error (show e)
    Right res -> pure res

signedInt :: Parser Int
signedInt = L.signed (pure ()) L.decimal
