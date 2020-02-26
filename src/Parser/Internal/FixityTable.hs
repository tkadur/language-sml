module Parser.Internal.FixityTable
  ( FixityTable()
  , Associativity
  , Precedence
  , addOperator
  , basisFixityTable
  , makeExprParser
  , operators
  )
where

import qualified Control.Monad.Combinators.Expr
                                               as E
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Relude.Unsafe                  ( (!!) )

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Ident                      ( Ident )
import qualified Ast.Ident                     as Ident
import           Parser.Internal.Basic          ( Parser
                                                , nothing
                                                , symbol
                                                )

type Associativity = Parser (Expr -> Expr -> Expr) -> E.Operator Parser Expr
type Precedence = Int

data FixityTable = FixityTable
    { table :: [[(Ident, E.Operator Parser Expr)]]
    , operators :: HashSet Ident
    }

makeExprParser :: Parser Expr -> FixityTable -> Parser Expr
makeExprParser expression FixityTable { table } =
  E.makeExprParser expression . reverse . stripIdents $ table
 where
  stripIdents :: [[(Ident, E.Operator Parser Expr)]]
              -> [[E.Operator Parser Expr]]
  stripIdents = fmap . fmap $ snd

addOperator :: Ident
            -> Associativity
            -> Precedence
            -> FixityTable
            -> FixityTable
addOperator ident@(Ident.Ident name) associativity precedence FixityTable { table, operators }
  = FixityTable { table = table', operators = operators' }
 where
  table' =
    -- Remove ident from the operator table
    let withoutIdent = filter (\(ident', _) -> ident' /= ident) <$> table
    -- Add ident to the operator table
    in  update precedence
               (infixExprOperator precedence associativity name :)
               withoutIdent

  operators' = HashSet.insert ident operators

basisFixityTable :: FixityTable
basisFixityTable = FixityTable
  { table     = [ infixExprOperator 0 E.InfixL <$> (basisOperators !! 0)
                , []
                , []
                , infixExprOperator 3 E.InfixL <$> (basisOperators !! 3)
                , infixExprOperator 4 E.InfixL <$> (basisOperators !! 4)
                , infixExprOperator 5 E.InfixR <$> (basisOperators !! 5)
                , infixExprOperator 6 E.InfixL <$> (basisOperators !! 6)
                , infixExprOperator 7 E.InfixL <$> (basisOperators !! 7)
                , []
                , []
                  -- Application
                , let separator = nothing
                      expr lhs rhs = Expr.App { Expr.lhs, Expr.rhs }
                  in  [(Ident.Ident "", E.InfixL (expr <$ separator))]
                ]
  , operators = HashSet.fromList (Ident.Ident <$> concat basisOperators)
  }
 where
  basisOperators =
    [
      -- 0
      ["before"]
      -- 1
    , []
      -- 2
    , []
      -- 3
    , [ ":="
      , "o"
      ]
      -- 4
    , [ "="
      , "<>"
      , ">"
      , ">="
      , "<"
      , "<="
      ]
      -- 5
    , [ "::"
      , "@"
      ]
      -- 6
    , [ "+"
      , "-"
      , "^"
      ]
      -- 7
    , [ "*"
      , "/"
      , "mod"
      , "div"
      ]
      -- 8
    , []
      -- 9
    , []
    ]

infixExprOperator :: Precedence
                  -> Associativity
                  -> Text
                  -> (Ident, E.Operator Parser Expr)
infixExprOperator precedence operator name =
  (Ident.Ident name, operator (expr <$ separator))
 where
  separator = symbol name
  expr lhs rhs = Expr.InfixApp { Expr.lhs
                               , Expr.op         = Ident.Ident name
                               , Expr.precedence
                               , Expr.rhs
                               }
