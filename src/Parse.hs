module Parse
  ( module P
  , parseFile
  , unsafeParseString
  , Parser
  , ParserOf
  , signedInt
  , L.decimal
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type ParserOf t = P.Parsec Void t

type Parser a = ParserOf Text a

parseFile :: Show e => P.Parsec e Text a -> String -> IO a
parseFile p f = do
  result <- P.parse p f <$> TIO.readFile f
  case result of
    Left e -> error (show e)
    Right res -> pure res

unsafeParseString :: Show e => P.Parsec e Text a -> String -> a
unsafeParseString p input =
  case P.parse p "<string>" (T.pack input) of
    Left e -> error (show e)
    Right res -> res

signedInt :: Parser Int
signedInt = L.signed (pure ()) L.decimal
