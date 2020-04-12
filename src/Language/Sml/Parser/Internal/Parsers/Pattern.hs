module Language.Sml.Parser.Internal.Parsers.Pattern where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                , sepBy
                                                )
import           Text.Megaparsec                ( try )

import           Language.Sml.Ast.Pat           ( MPat )
import qualified Language.Sml.Ast.Pat          as Pat
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
                                         hiding ( Parser )
import           Language.Sml.Parser.Internal.FixityTable
                                                ( FixityTable )
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import           Language.Sml.Parser.Internal.Parsers.Identifier
                                                ( nonfixLongValueIdentifier
                                                , valueIdentifier
                                                , label
                                                , op
                                                )
import           Language.Sml.Parser.Internal.Parsers.Literal
                                                ( literal )
import           Language.Sml.Parser.Internal.Parsers.Type
                                                ( typ )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

-- | Parses a pattern
pattern :: (MonadParser parser) => FixityTable -> parser MPat
pattern fixityTable = dbg ["pattern"] $ do
  -- Handle left-recursive type annotations
  pat         <- infixed
  maybeAnnots <- many (token_ Token.Colon >> typ)
  return $ case nonEmpty maybeAnnots of
    Nothing -> pat
    Just annots ->
      (foldl' (\p t -> Marked.merge p t (Pat.Annot p t)) pat annots)
 where

  -- Handle infix operators
  infixed     = FixityTable.makeParser FixityTable.Pat pattern' fixityTable

  -- Non-left recursive cases
  pattern'    = choice [constructed, asPat, atomicPattern fixityTable]

  -- Constructor application
  -- @try@ to prevent failure from trying to parse variable as constructor
  constructed = dbg ["pattern", "constructed"] . marked . try $ do
    constructor <- nonfixLongValueIdentifier fixityTable
    arg         <- atomicPattern fixityTable
    return Pat.Constructed { Pat.constructor, Pat.arg }

  -- @try@ to prevent failure from trying to parse variable as ident
  asPat = marked . try $ do
    ident <- op valueIdentifier
    annot <- optional (token_ Token.Colon >> typ)
    token_ Token.As
    as <- pattern fixityTable
    return Pat.As { Pat.ident, Pat.annot, Pat.as }

atomicPattern :: forall parser
               . (MonadParser parser)
              => FixityTable
              -> parser MPat
atomicPattern fixityTable = choice
  [wild, lit, vident, record, parens, tup, lst]
 where

  -- Wildcard
  wild = dbg ["pattern", "wild"] . marked $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit = dbg ["pattern", "lit"] . marked $ Pat.Lit <$> literal

  -- Value identifier
  -- @try@ to prevent failure from trying to parse infix operator as ident
  vident = dbg ["pattern", "ident"] . marked $ try
    (Pat.Ident <$> nonfixLongValueIdentifier fixityTable)

  -- Record
  record = dbg ["typ", "record"] . marked $ Pat.Record <$> braces
    (row `sepBy` token_ Token.Comma)
   where
    row :: (MonadParser parser) => parser Pat.MRow
    row     = marked $ choice [rowWild, regularRow, rowPun]

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
  tup    = marked $ Pat.Tuple <$> tuple (pattern fixityTable)

  -- List
  lst    = marked $ Pat.List <$> list (pattern fixityTable)
