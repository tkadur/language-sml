module Language.Sml.Parser.Internal.Parsers.Expression where

import           Control.Monad.Combinators      ( choice
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( sepBy1 )
import           Text.Megaparsec                ( try )

import           Language.Sml.Ast.Expr          ( MExpr )
import qualified Language.Sml.Ast.Expr         as Expr
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.Combinators
                                                ( sepBy2 )
import           Language.Sml.Parser.Internal.FixityTable
                                                ( FixityTable )
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import {-# SOURCE #-} Language.Sml.Parser.Internal.Parsers.Declaration

import           Language.Sml.Parser.Internal.Parsers.Identifier
                                                ( nonfixLongValueIdentifier
                                                , label
                                                )
import           Language.Sml.Parser.Internal.Parsers.Literal
                                                ( literal )
import           Language.Sml.Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Language.Sml.Parser.Internal.Parsers.Type
                                                ( typ )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

-- | Parses an expression
expression :: (MonadParser parser) => FixityTable -> parser MExpr
expression fixityTable = dbg ["expression"] $ do
  -- Handle left-recursive cases
  expr <- expression'
  next <- optional $ choice
    (map (\t -> token_ t >> return t)
         [Token.Colon, Token.Andalso, Token.Orelse, Token.Handle]
    )
  case next of
    Nothing          -> return expr
    Just Token.Colon -> do
      t <- typ
      return $ Marked.merge expr t (Expr.Annot { Expr.expr, Expr.typ = t })
    Just Token.Andalso -> do
      rhs <- expression fixityTable
      return
        $ Marked.merge expr rhs (Expr.Andalso { Expr.lhs = expr, Expr.rhs })
    Just Token.Orelse -> do
      rhs <- expression fixityTable
      return $ Marked.merge expr rhs (Expr.Orelse { Expr.lhs = expr, Expr.rhs })
    Just Token.Handle -> do
      m <- match fixityTable
      return $ Marked.merge expr
                            ((\Expr.MatchArm { Expr.rhs } -> rhs) $ last m)
                            (Expr.Handle { Expr.expr, Expr.match = m })
    Just t -> error $ "invalid following token " <> show t
 where
  -- Non-left recursive cases
  expression' = choice [infexp, raise, ifThenElse, while, caseof, fn]

  infexp      = FixityTable.makeParser FixityTable.Expr
                                       (atomicExpression fixityTable)
                                       fixityTable

  raise = marked $ do
    token_ Token.Raise
    expr <- expression fixityTable
    return $ Expr.Raise expr

  ifThenElse = marked $ do
    token_ Token.If
    cond <- expression fixityTable
    token_ Token.Then
    ifExpr <- expression fixityTable
    token_ Token.Else
    elseExpr <- expression fixityTable
    return Expr.If { Expr.cond, Expr.ifExpr, Expr.elseExpr }

  while = marked $ do
    token_ Token.While
    cond <- expression fixityTable
    token_ Token.Do
    body <- expression fixityTable
    return Expr.While { Expr.cond, Expr.body }

  caseof = marked $ do
    token_ Token.Case
    expr <- expression fixityTable
    token_ Token.Of
    m <- match fixityTable
    return Expr.Case { Expr.expr, Expr.match = m }

  fn = marked $ do
    token_ Token.Fn
    m <- match fixityTable
    return Expr.Fn { Expr.match = m }

atomicExpression :: (MonadParser parser) => FixityTable -> parser MExpr
atomicExpression fixityTable = dbg ["expression", "atomicExpression"] $ choice
  [lit, vident, record, recordSelector, tup, lst, sqnc, letInEnd, parens]
 where
  lit    = marked $ Expr.Lit <$> literal

  -- @try@ to prevent failure from trying to parse infix operator as bareIdentifier
  vident = marked . try $ Expr.Ident <$> nonfixLongValueIdentifier fixityTable

  record = marked $ Expr.Record <$> braces (row `sepBy` token_ Token.Comma)
   where
    row :: (MonadParser parser) => parser Expr.Row
    row = do
      lab <- label
      token_ Token.Equal
      expr <- expression fixityTable
      return Expr.Row { Expr.label = lab, Expr.expr }

  recordSelector = marked $ do
    token_ Token.Octothorpe
    lab <- label
    return $ Expr.RecordSelector lab

  -- @try@ to prevent conflict with sqnc/parens
  tup  = marked . try $ Expr.Tuple <$> tuple (expression fixityTable)

  lst  = marked $ Expr.List <$> list (expression fixityTable)

  -- @try@ to prevent conflict with parens
  sqnc = marked . try $ Expr.Sequence <$> parenthesized
    (expression fixityTable `sepBy2` token_ Token.Semicolon)

  letInEnd = marked $ do
    token_ Token.Let
    decl <- evalStateT declaration fixityTable
    token_ Token.In
    exprs <- expression fixityTable `sepBy1` token_ Token.Semicolon
    token_ Token.End
    return Expr.Let { Expr.decl, Expr.exprs }

  parens = parenthesized (expression fixityTable)

match :: (MonadParser parser) => FixityTable -> parser Expr.Match
match fixityTable = matchArm fixityTable `sepBy1` token_ Token.Pipe

matchArm :: (MonadParser parser) => FixityTable -> parser Expr.MatchArm
matchArm fixityTable = do
  lhs <- pattern fixityTable
  token_ Token.Widearrow
  rhs <- expression fixityTable
  return Expr.MatchArm { Expr.rhs, Expr.lhs }
