module Language.Sml.Parser.Internal.Parsers.Expression where

import           Control.Monad.Combinators      ( choice
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( sepBy1 )
import           Text.Megaparsec                ( lookAhead
                                                , observing
                                                , try
                                                )

import           Language.Sml.Ast.Expr          ( MExpr )
import qualified Language.Sml.Ast.Expr         as Expr
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.Combinators
                                                ( sepBy2 )
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
expression :: Parser MExpr
expression = dbg ["expression"] $ do
  -- Handle left-recursive cases
  expr <- expression'
  next <- optional $ choice
    (map (\t -> token_ t >> return t)
         [Token.Colon, Token.Andalso, Token.Orelse, Token.Handle]
    )
  -- Manual lookahead
  case next of
    Nothing          -> return expr
    Just Token.Colon -> do
      t <- typ
      return $ Marked.merge expr t (Expr.Annot { Expr.expr, Expr.typ = t })
    Just Token.Andalso -> do
      -- Check if next token is paren
      -- TODO(tkadur) this is a terribly hacky way to handle precedence
      parens <-
        either (const False) (const True)
          <$> (observing . lookAhead . try $ parenthesized expression)
      rhs <- expression

      -- Fix precedence issues with these manual lookahead cases
      -- In particular, if @expr@ is an @Expr.Orelse@ or @Expr.Handle@
      -- with no parens around it, we will mistakenly parse those constructs
      -- as having higher precedence than @Expr.Andalso@
      return . Marked.merge expr rhs $ case (parens, Marked.value rhs) of
        (False, Expr.Orelse { Expr.lhs = lhs', Expr.rhs = rhs' }) ->
          Expr.Orelse
            { Expr.lhs = Marked.merge
                           expr
                           lhs'
                           (Expr.Andalso { Expr.lhs = expr, Expr.rhs = lhs' })
            , Expr.rhs = rhs'
            }
        (False, Expr.Handle { Expr.expr = expr', Expr.match = match' }) ->
          Expr.Handle
            { Expr.expr  = Marked.merge
                             expr
                             expr'
                             (Expr.Andalso { Expr.lhs = expr, Expr.rhs = expr' })
            , Expr.match = match'
            }
        _ -> Expr.Andalso { Expr.lhs = expr, Expr.rhs }
    Just Token.Orelse -> do
      -- See @Token.Andalso@ case for explanation
      parens <-
        either (const False) (const True)
          <$> (observing . lookAhead . try $ parenthesized expression)
      rhs <- expression

      return $ Marked.merge expr rhs $ case (parens, Marked.value rhs) of
        (False, Expr.Handle { Expr.expr = expr', Expr.match = match' }) ->
          Expr.Handle
            { Expr.expr  = Marked.merge
                             expr
                             expr'
                             (Expr.Orelse { Expr.lhs = expr, Expr.rhs = expr' })
            , Expr.match = match'
            }
        _ -> Expr.Orelse { Expr.lhs = expr, Expr.rhs }
    Just Token.Handle -> do
      m <- match
      return $ Marked.merge expr
                            ((\Expr.MatchArm { Expr.rhs } -> rhs) $ last m)
                            (Expr.Handle { Expr.expr, Expr.match = m })
    Just t -> error $ "invalid following token " <> show t
 where
  -- Non-left recursive cases
  expression' = choice [infexp, raise, ifThenElse, while, caseof, fn]

  infexp      = do
    fixityTable <- get
    FixityTable.makeParser FixityTable.Expr atomicExpression fixityTable

  raise = marked $ do
    token_ Token.Raise
    expr <- expression
    return $ Expr.Raise expr

  ifThenElse = marked $ do
    token_ Token.If
    cond <- expression
    token_ Token.Then
    ifExpr <- expression
    token_ Token.Else
    elseExpr <- expression
    return Expr.If { Expr.cond, Expr.ifExpr, Expr.elseExpr }

  while = marked $ do
    token_ Token.While
    cond <- expression
    token_ Token.Do
    body <- expression
    return Expr.While { Expr.cond, Expr.body }

  caseof = marked $ do
    token_ Token.Case
    expr <- expression
    token_ Token.Of
    m <- match
    return Expr.Case { Expr.expr, Expr.match = m }

  fn = marked $ do
    token_ Token.Fn
    m <- match
    return Expr.Fn { Expr.match = m }

atomicExpression :: Parser MExpr
atomicExpression = dbg ["expression", "atomicExpression"] $ choice
  [lit, vident, record, recordSelector, tup, lst, sqnce, letInEnd, parens]
 where
  lit    = marked $ Expr.Lit <$> literal

  -- @try@ to prevent failure from trying to parse infix operator as bareIdentifier
  vident = marked . try $ Expr.Ident <$> nonfixLongValueIdentifier

  record = marked $ Expr.Record <$> braces (row `sepBy` token_ Token.Comma)
   where
    row :: Parser Expr.Row
    row = do
      lab <- label
      token_ Token.Equal
      expr <- expression
      return Expr.Row { Expr.label = lab, Expr.expr }

  recordSelector = marked $ do
    token_ Token.Octothorpe
    lab <- label
    return $ Expr.RecordSelector lab

  -- @try@ to prevent conflict with sqnce/parens
  tup   = marked . try $ Expr.Tuple <$> tuple expression

  lst   = marked $ Expr.List <$> list expression

  -- @try@ to prevent conflict with parens
  sqnce = marked . try $ Expr.Sequence <$> parenthesized
    (expression `sepBy2` token_ Token.Semicolon)

  letInEnd = marked $ do
    token_ Token.Let
    decl <- do
      -- Save old fixity table
      fixityTable <- get
      res         <- declaration
      -- Restore old fixity table
      put fixityTable
      return res
    token_ Token.In
    exprs <- expression `sepBy1` token_ Token.Semicolon
    token_ Token.End
    return Expr.Let { Expr.decl, Expr.exprs }

  parens = parenthesized expression

match :: Parser Expr.Match
match = matchArm `sepBy1` token_ Token.Pipe

matchArm :: Parser Expr.MatchArm
matchArm = do
  lhs <- pattern
  token_ Token.Widearrow
  rhs <- expression
  return Expr.MatchArm { Expr.rhs, Expr.lhs }
