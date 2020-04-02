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
                                                ( nonfixValueIdentifier
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
pattern fixityTable = dbg ["pattern"] $ maybeAnnot
 where
  -- Handle left-recursive type annotations
  maybeAnnot = dbg ["pattern", "maybeAnnot"] $ do
    pat         <- pattern'
    maybeAnnots <- optional $ some (token_ Token.Colon >> typ)
    return $ case maybeAnnots of
      Nothing -> pat
      Just (annot :| annots) ->
        foldl' Pat.Annot (Pat.Annot { Pat.pat, Pat.typ = annot }) annots

  pattern'    = FixityTable.makeParser FixityTable.Pat pattern'' fixityTable
  pattern''   = choice [constructed, asPat, atomicPattern fixityTable]

  -- Constructor application
  -- @try@ to prevent failure from trying to parse variable as constructor
  constructed = dbg ["pattern", "constructed"] . try $ do
    constructor <- nonfixValueIdentifier fixityTable
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
  [wild, lit, vident, record, tup, lst, parens]
 where

  -- Wildcard
  wild   = dbg ["pattern", "wild"] $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit    = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- Value identifier
  vident = dbg ["pattern", "ident"]
    -- @try@ to prevent failure from trying to parse infix operator as ident
    $ try (Pat.Ident <$> nonfixValueIdentifier fixityTable)

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

  -- Tuple
  tup    = Pat.Tuple <$> tuple (pattern fixityTable)

  -- List
  lst    = Pat.List <$> list (pattern fixityTable)

  -- Parenthesized
  -- @try@ to prevent failure from consuming the start of a tuple
  parens = try $ parenthesized (pattern fixityTable)
