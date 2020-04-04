module Parser.Internal.Parsers.Literal where

import           Control.Monad.Combinators      ( choice )
import           Data.Scientific                ( Scientific )

import           Ast.Lit.Character              ( Character )
import           Ast.Lit                        ( Lit )
import qualified Ast.Lit                       as Lit
import           Parser.Internal.Basic
import qualified Parser.Internal.Token         as Token

-- | Parses a numerical literal
literal :: (MonadParser parser) => parser Lit
literal = dbg ["literal"] $ choice
  [ Lit.Int <$> decimal
  , Lit.Hex <$> hexadecimal
  , Lit.Word <$> word
  , Lit.HexWord <$> hexword
  , Lit.Real <$> real
  , Lit.Char <$> char
  , Lit.String <$> string
  ]

string :: (MonadParser parser) => parser [Character]
string = dbg ["string"] $ tokenWith
  (\case
    Token.String cs -> Just cs
    _ -> Nothing
  )

char :: (MonadParser parser) => parser [Character]
char = dbg ["char"] $ tokenWith
  (\case
    Token.Character cs -> Just cs
    _ -> Nothing
  )

-- | Parses a decimal integer literal
decimal :: (MonadParser parser) => parser Integer
decimal = dbg ["decimal"] $ tokenWith
  (\case
    Token.Int i -> Just i
    _           -> Nothing
  )

-- | Parses a hexadecimal integer literal
hexadecimal :: (MonadParser parser) => parser Integer
hexadecimal = dbg ["hexadecimal"] $ tokenWith
  (\case
    Token.Hex i -> Just i
    _           -> Nothing
  )

-- | Parses a word literal
word :: (MonadParser parser) => parser Integer
word = dbg ["word"] $ tokenWith
  (\case
    Token.Word i -> Just i
    _ -> Nothing
  )

-- | Parses a hexadecimal word literal
hexword :: (MonadParser parser) => parser Integer
hexword = dbg ["hexword"] $ tokenWith
  (\case
    Token.HexWord i -> Just i
    _ -> Nothing
  )

-- | Parses a real number
real :: (MonadParser parser) => parser Scientific
real = dbg ["real"] $ tokenWith
  (\case
    Token.Real n -> Just n
    _ -> Nothing
  )
