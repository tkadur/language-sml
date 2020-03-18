module Parser.Internal.Parsers.Expression where

import           Control.Monad.Combinators      ( choice )
import           Control.Monad.Combinators.NonEmpty
                                                ( sepBy1 )
import           Text.Megaparsec                ( try )

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( nonfixValueIdentifier )
import           Parser.Internal.Parsers.Literal
                                                ( literal )
import           Parser.Internal.Parsers.Pattern
                                                ( pattern )
import qualified Parser.Internal.Token         as Token

-- | Parses an expression
expression :: FixityTable -> Parser Expr
expression fixityTable = dbg ["expression"]
  $ FixityTable.makeParser FixityTable.Expr expression' fixityTable
 where
  expression' = choice
    [ lit
    , var
    -- @try@ to prevent failure from consuming the start of a tuple
    , try . parenthesized $ expression fixityTable
    , tup
    , lst
    , fn
    , caseof
    ]

  lit = Expr.Lit <$> literal

  -- @try@ to prevent failure from trying to parse infix operator as bareIdentifier
  var = try $ (Expr.Var <$> nonfixValueIdentifier fixityTable)

  tup = Expr.Tuple <$> tuple (expression fixityTable)

  lst = Expr.List <$> list (expression fixityTable)

  fn  = do
    token_ Token.Fn
    m <- match fixityTable
    return Expr.Fn { Expr.match = m }

  caseof = do
    token_ Token.Case
    expr <- expression fixityTable
    token_ Token.Of
    m <- match fixityTable
    return Expr.Case { Expr.expr, Expr.match = m }

match :: FixityTable -> Parser Expr.Match
match fixityTable = matchArm fixityTable `sepBy1` token_ Token.Pipe

matchArm :: FixityTable -> Parser Expr.MatchArm
matchArm fixityTable = do
  lhs <- pattern fixityTable
  token_ Token.Widearrow
  rhs <- expression fixityTable
  return Expr.MatchArm { Expr.rhs, Expr.lhs }
