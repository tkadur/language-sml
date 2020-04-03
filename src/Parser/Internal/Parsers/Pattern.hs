module Parser.Internal.Parsers.Pattern where

import           Control.Monad.Combinators      ( choice
                                                , sepBy
                                                )
import           Control.Applicative.Combinators.NonEmpty
                                                ( some )
import           Text.Megaparsec                ( try )

import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( nonfixLongValueIdentifier
                                                , valueIdentifier
                                                , label
                                                , op
                                                )
import           Parser.Internal.Parsers.Literal
                                                ( literal )
import           Parser.Internal.Parsers.Type   ( typ )
import qualified Parser.Internal.Token         as Token

-- | Parses a pattern
pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg ["pattern"] $ do
  -- Handle left-recursive type annotations
  pat         <- infixed
  maybeAnnots <- optional $ some (token_ Token.Colon >> typ)
  return $ case maybeAnnots of
    Nothing -> pat
    Just (annot :| annots) ->
      foldl' Pat.Annot (Pat.Annot { Pat.pat, Pat.typ = annot }) annots
 where

  -- Handle infix operators
  infixed     = FixityTable.makeParser FixityTable.Pat pattern' fixityTable

  -- Non-left recursive cases
  pattern'    = choice [constructed, asPat, atomicPattern fixityTable]

  -- Constructor application
  -- @try@ to prevent failure from trying to parse variable as constructor
  constructed = dbg ["pattern", "constructed"] . try $ do
    constructor <- nonfixLongValueIdentifier fixityTable
    arg         <- atomicPattern fixityTable
    return Pat.Constructed { Pat.constructor, Pat.arg }

  -- @try@ to prevent failure from trying to parse variable as ident
  asPat = try $ do
    ident <- op valueIdentifier
    annot <- optional (token_ Token.Colon >> typ)
    token_ Token.As
    as <- pattern fixityTable
    return Pat.As { Pat.ident, Pat.annot, Pat.as }

atomicPattern :: FixityTable -> Parser Pat
atomicPattern fixityTable = choice
  [wild, lit, vident, record, parens, tup, lst]
 where

  -- Wildcard
  wild   = dbg ["pattern", "wild"] $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit    = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- Value identifier
  vident = dbg ["pattern", "ident"]
    -- @try@ to prevent failure from trying to parse infix operator as ident
    $ try (Pat.Ident <$> nonfixLongValueIdentifier fixityTable)

  -- Record
  record = dbg ["typ", "record"] $ Pat.Record <$> braces
    (row `sepBy` token_ Token.Comma)
   where
    row :: Parser Pat.Row
    row     = choice [rowWild, regularRow, rowPun]

    rowWild = do
      token_ Token.Dotdotdot
      return Pat.RowWild

    -- @try@ to prevent failure from tying to parse pun ident as label
    regularRow = try $ do
      lbl <- label
      token_ Token.Equal
      pat <- pattern fixityTable
      return Pat.Row { Pat.label = lbl, Pat.pat }

    rowPun = do
      ident <- valueIdentifier
      annot <- optional (token_ Token.Colon >> typ)
      as    <- optional (token_ Token.As >> pattern fixityTable)
      return Pat.RowPun { Pat.ident, Pat.annot, Pat.as }

  -- Parenthesized
  -- @try@ to prevent failure from consuming the start of a tuple
  parens = try $ parenthesized (pattern fixityTable)

  -- Tuple
  tup    = Pat.Tuple <$> tuple (pattern fixityTable)

  -- List
  lst    = Pat.List <$> list (pattern fixityTable)
