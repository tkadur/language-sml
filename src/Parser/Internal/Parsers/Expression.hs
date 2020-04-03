module Parser.Internal.Parsers.Expression where

import           Control.Monad.Combinators      ( choice
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( sepBy1 )
import           Text.Megaparsec                ( try )

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import {-# SOURCE #-} Parser.Internal.Parsers.Declaration

import           Parser.Internal.Parsers.Identifier
                                                ( nonfixValueIdentifier
                                                , label
                                                )
import           Parser.Internal.Parsers.Literal
                                                ( literal )
import           Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Parser.Internal.Parsers.Type   ( typ )
import qualified Parser.Internal.Token         as Token

-- | Parses an expression
expression :: FixityTable -> Parser Expr
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
      return Expr.Annot { Expr.expr, Expr.typ = t }
    Just Token.Andalso -> do
      rhs <- expression fixityTable
      return Expr.Andalso { Expr.lhs = expr, Expr.rhs }
    Just Token.Orelse -> do
      rhs <- expression fixityTable
      return Expr.Orelse { Expr.lhs = expr, Expr.rhs }
    Just Token.Handle -> do
      m <- match fixityTable
      return Expr.Handle { Expr.expr, Expr.match = m }
    Just t -> error $ "invalid following token " <> show t
 where
  -- Non-left recursive cases
  expression' = choice [infexp, raise, ifThenElse, while, caseof, fn]

  infexp      = FixityTable.makeParser FixityTable.Expr
                                       (atomicExpression fixityTable)
                                       fixityTable

  raise = do
    token_ Token.Raise
    expr <- expression fixityTable
    return $ Expr.Raise expr

  ifThenElse = do
    token_ Token.If
    cond <- expression fixityTable
    token_ Token.Then
    ifExpr <- expression fixityTable
    token_ Token.Else
    elseExpr <- expression fixityTable
    return Expr.If { Expr.cond, Expr.ifExpr, Expr.elseExpr }

  while = do
    token_ Token.While
    cond <- expression fixityTable
    token_ Token.Do
    body <- expression fixityTable
    return Expr.While { Expr.cond, Expr.body }

  caseof = do
    token_ Token.Case
    expr <- expression fixityTable
    token_ Token.Of
    m <- match fixityTable
    return Expr.Case { Expr.expr, Expr.match = m }

  fn = do
    token_ Token.Fn
    m <- match fixityTable
    return Expr.Fn { Expr.match = m }

atomicExpression :: FixityTable -> Parser Expr
atomicExpression fixityTable = dbg ["expression", "atomicExpression"] $ choice
  [lit, vident, record, recordSelector, tup, lst, sqnc, letInEnd, parens]
 where
  lit    = Expr.Lit <$> literal

  -- @try@ to prevent failure from trying to parse infix operator as bareIdentifier
  vident = try $ Expr.Ident <$> nonfixValueIdentifier fixityTable

  record = Expr.Record <$> braces (row `sepBy` token_ Token.Comma)
   where
    row :: Parser Expr.Row
    row = do
      lab <- label
      token_ Token.Equal
      expr <- expression fixityTable
      return Expr.Row { Expr.label = lab, Expr.expr }

  recordSelector = do
    token_ Token.Octothorpe
    lab <- label
    return $ Expr.RecordSelector lab

  -- @try@ to prevent conflict with sqnc/parens
  tup  = try $ Expr.Tuple <$> tuple (expression fixityTable)

  lst  = Expr.List <$> list (expression fixityTable)

  -- @try@ to prevent conflict with parens
  sqnc = try $ Expr.Sequence <$> parenthesized
    (expression fixityTable `sepBy1` token_ Token.Semicolon)

  letInEnd = do
    token_ Token.Let
    decl <- evalStateT declaration fixityTable
    token_ Token.In
    exprs <- expression fixityTable `sepBy1` token_ Token.Semicolon
    token_ Token.End
    return Expr.Let { Expr.decl, Expr.exprs }

  parens = parenthesized (expression fixityTable)

match :: FixityTable -> Parser Expr.Match
match fixityTable = matchArm fixityTable `sepBy1` token_ Token.Pipe

matchArm :: FixityTable -> Parser Expr.MatchArm
matchArm fixityTable = do
  lhs <- pattern fixityTable
  token_ Token.Widearrow
  rhs <- expression fixityTable
  return Expr.MatchArm { Expr.rhs, Expr.lhs }
