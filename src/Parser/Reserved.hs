module Parser.Reserved where

import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C

import           Parser.Basic

reservedTokens :: [Text]
reservedTokens = reservedWords ++ reservedOps
 where
  reservedWords = text <$> enumFrom (toEnum 0 :: ReservedWords)
  reservedOps   = text <$> enumFrom (toEnum 0 :: ReservedOps)

class Reserved a where
  text :: a -> Text
  charSet :: a -> Parser Char

data ReservedWords
  = Val
  deriving (Show, Enum)

data ReservedOps
  = Equal
  deriving (Show, Enum)

instance Reserved ReservedWords where
  text Val = "val"
  charSet _ = C.letterChar <|> C.char '_'

instance Reserved ReservedOps where
  text Equal = "="
  charSet _ = C.symbolChar <|> C.char '_'
