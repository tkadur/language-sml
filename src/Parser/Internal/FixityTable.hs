module Parser.Internal.FixityTable
  ( FixityTable()
  , Associativity
  , Precedence
  , addOperator
  , removeOperator
  , basisFixityTable
  , makeExprParser
  , operators
  )
where

import qualified Control.Monad.Combinators.Expr
                                               as E
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

type Table = [[(Ident, E.Operator Parser Expr)]]

data FixityTable = FixityTable
    { table :: Table
    , operators :: HashSet Ident
    }

makeExprParser :: Parser Expr -> FixityTable -> Parser Expr
makeExprParser expression FixityTable { table } =
  table
    |> stripIdents
    -- We store the table in ascending precedence order,
    -- but E.makeExprParser expects it in descending order
    |> reverse
    |> E.makeExprParser expression
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
    table
      |> removeOperatorFromTable ident
      -- Add ident to the operator table
      |> update precedence (infixExprOperator precedence associativity name :)

  operators' = HashSet.insert ident operators

removeOperator :: Ident -> FixityTable -> FixityTable
removeOperator ident FixityTable { table, operators } = FixityTable
  { table     = table'
  , operators = operators'
  }
 where
  table'     = removeOperatorFromTable ident table
  operators' = HashSet.delete ident operators

removeOperatorFromTable :: Ident -> Table -> Table
removeOperatorFromTable ident = map $ filter (\(ident', _) -> ident' /= ident)

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
  , operators = HashSet.fromList $ map Ident.Ident (concat basisOperators)
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
