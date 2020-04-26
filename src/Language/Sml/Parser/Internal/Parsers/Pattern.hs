module Language.Sml.Parser.Internal.Parsers.Pattern where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                , sepBy
                                                , sepBy1
                                                )
import           Text.Megaparsec                ( try )

import           Language.Sml.Ast.Pat           ( MPat )
import qualified Language.Sml.Ast.Pat          as Pat
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
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
pattern :: Parser MPat
pattern = dbg ["pattern"] $ do
  -- Handle left-recursive type annotations
  pat         <- infixed
  maybeAnnots <- many (token_ Token.Colon >> typ)
  return $ case nonEmpty maybeAnnots of
    Nothing -> pat
    Just annots ->
      (foldl' (\p t -> Marked.merge p t (Pat.Annot p t)) pat annots)
 where

  -- Handle infix operators
  infixed = do
    fixityTable <- get
    FixityTable.makeParser FixityTable.Pat pattern' fixityTable

  -- Non-left recursive cases
  pattern'    = choice [constructed, asPat, atomicPattern]

  -- Constructor application
  -- @try@ to prevent failure from trying to parse variable as constructor
  constructed = dbg ["pattern", "constructed"] . marked . try $ do
    constructor <- nonfixLongValueIdentifier
    arg         <- atomicPattern
    return Pat.Constructed { Pat.constructor, Pat.arg }

  -- @try@ to prevent failure from trying to parse variable as ident
  asPat = marked . try $ do
    ident <- op valueIdentifier
    annot <- optional (token_ Token.Colon >> typ)
    token_ Token.As
    as <- pattern
    return Pat.As { Pat.ident, Pat.annot, Pat.as }

atomicPattern :: Parser MPat
atomicPattern = choice [wild, lit, vident, record, tupOrParens, lst]
 where

  -- Wildcard
  wild = dbg ["pattern", "wild"] . marked $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit = dbg ["pattern", "lit"] . marked $ Pat.Lit <$> literal

  -- Value identifier
  vident = dbg ["pattern", "ident"] $ do
    ident <- nonfixLongValueIdentifier
    return $ Marked.replace ident (Pat.Ident ident)

  -- Record
  record = dbg ["typ", "record"] . marked $ Pat.Record <$> braces
    (row `sepBy` token_ Token.Comma)
   where
    row :: Parser Pat.MRow
    row     = marked $ choice [rowWild, regularRow, rowPun]

    rowWild = do
      token_ Token.Dotdotdot
      return Pat.RowWild

    -- @try@ to prevent failure from tying to parse pun ident as label
    regularRow = do
      lbl <- try (label << token_ Token.Equal)
      pat <- pattern
      return Pat.Row { Pat.label = lbl, Pat.pat }

    rowPun = do
      ident <- valueIdentifier
      annot <- optional (token_ Token.Colon >> typ)
      as    <- optional (token_ Token.As >> pattern)
      return Pat.RowPun { Pat.ident, Pat.annot, Pat.as }

  -- For efficiency reasons we
  --   - do manual lookahead to avoid overusing @try@
  --   - Use span information from the surrounding parens to avoid overusing @marked@
  tupOrParens = do
    l        <- marked (token_ Token.Lparen)
    maybePat <- optional pattern
    let expected = [Token.Rparen, Token.Comma]
    next <- marked
      $ tokenWith (\t -> if t `elem` expected then Just t else Nothing)

    (pat, r) <- case (maybePat, Marked.value next) of
      (Nothing , Token.Rparen) -> return (Pat.Tuple [], void next)
      (Just pat, Token.Rparen) -> return (Marked.value pat, void next)
      (Just pat, Token.Comma ) -> do
        pats <- pattern `sepBy1` token_ Token.Comma
        r    <- marked (token_ Token.Rparen)
        return (Pat.Tuple (pat : pats), r)
      (_, t)
        | t `elem` expected -> fail $ "invalid following token " <> show t
        | otherwise         -> error $ "Impossible token " <> show t

    return $ Marked.merge l r pat

  -- List
  lst = marked $ Pat.List <$> list pattern
