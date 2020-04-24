module Language.Sml.Parser.Internal.Parsers.Literal where

import           Control.Monad.Combinators      ( choice )
import           Data.Scientific                ( Scientific )

import           Language.Sml.Ast.Lit.Character ( Character )
import           Language.Sml.Ast.Lit           ( Lit )
import qualified Language.Sml.Ast.Lit          as Lit
import           Language.Sml.Common.Positive   ( Positive )
import           Language.Sml.Parser.Internal.Basic
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

-- | Parses a numerical literal
literal :: Parser Lit
literal = dbg ["literal"] $ choice
  [ Lit.Int <$> decimal
  , Lit.Hex <$> hexadecimal
  , Lit.Word <$> word
  , Lit.HexWord <$> hexword
  , Lit.Real <$> real
  , Lit.Char <$> char
  , Lit.String <$> string
  ]

string :: Parser [Character]
string = dbg ["string"] . tokenWith $ \case
  Token.String cs -> Just cs
  _ -> Nothing

char :: Parser [Character]
char = dbg ["char"] . tokenWith $ \case
  Token.Character cs -> Just cs
  _ -> Nothing

-- | Parses a decimal integer literal
decimal :: Parser Integer
decimal = dbg ["decimal"] . tokenWith $ \case
  Token.Int i -> Just i
  _           -> Nothing

-- | Parses a hexadecimal integer literal
hexadecimal :: Parser Integer
hexadecimal = dbg ["hexadecimal"] . tokenWith $ \case
  Token.Hex i -> Just i
  _           -> Nothing

-- | Parses a word literal
word :: Parser Positive
word = dbg ["word"] . tokenWith $ \case
  Token.Word i -> Just i
  _ -> Nothing

-- | Parses a hexadecimal word literal
hexword :: Parser Positive
hexword = dbg ["hexword"] . tokenWith $ \case
  Token.HexWord i -> Just i
  _ -> Nothing

-- | Parses a real number
real :: Parser Scientific
real = dbg ["real"] . tokenWith $ \case
  Token.Real n -> Just n
  _ -> Nothing
