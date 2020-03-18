module Parser.Internal.FixityTable
  ( FixityTable()
  , Associativity
  , Precedence
  , Operators
  , Infixable(..)
  , addOperators
  , removeOperators
  , basisFixityTable
  , makeParser
  , operators
  )
where

import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import           Relude.Unsafe                  ( (!!) )
import qualified Text.Show

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import           Ast.Ident.Ident                ( Ident )
import qualified Ast.Ident.Ident               as Ident
import           Parser.Internal.Basic
import qualified Parser.Internal.Token         as Token

type Associativity = forall a . Parser (a -> a -> a) -> E.Operator Parser a

type Precedence = Int

type TableEntry = (Ident, E.Operator Parser Pat, E.Operator Parser Expr)

type Table = [[TableEntry]]

type Operators = HashSet Ident

data FixityTable = FixityTable
  { table :: Table
  , operators :: Operators
  }

instance Show FixityTable where
  show FixityTable { table, operators } =
    (  table
      |> liftTable (\(ident, _, _) -> ident)
      |> map (\idents -> show idents <> "\n")
      |> concat
      )
      <> show operators
      <> "\n"

data Infixable a where
  Pat ::Infixable Pat
  Expr ::Infixable Expr

makeParser :: Infixable a -> Parser a -> FixityTable -> Parser a
makeParser infixable parser FixityTable { table } =
  table
    |> getTable infixable
    -- We store the table in ascending precedence order,
    -- but E.makeExprParser expects it in descending order
    |> reverse
    |> E.makeExprParser parser

getTable :: Infixable a -> Table -> [[E.Operator Parser a]]
getTable infixable = case infixable of
  Pat  -> getPatTable
  Expr -> getExprTable
 where
  getPatTable :: Table -> [[E.Operator Parser Pat]]
  getPatTable =
    -- Patterns cannot contain infix "="
    removeOperatorFromTable (Ident.Ident "=")
      >>> (liftTable $ \(_, patTable, _) -> patTable)

  getExprTable :: Table -> [[E.Operator Parser Expr]]
  getExprTable = liftTable $ \(_, _, exprTable) -> exprTable

-- | Lifts a function over `TableEntry`s to a function over `Table`s
liftTable :: (TableEntry -> a) -> (Table -> [[a]])
liftTable = fmap . fmap

addOperators :: NonEmpty Ident
             -> Associativity
             -> Precedence
             -> FixityTable
             -> FixityTable
addOperators idents associativity precedence =
  idents
    |> NonEmpty.map (\ident -> addOperator ident associativity precedence)
    |> foldl' (.) id

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

removeOperators :: NonEmpty Ident -> FixityTable -> FixityTable
removeOperators = foldl' (.) id . NonEmpty.map removeOperator

removeOperator :: Ident -> FixityTable -> FixityTable
removeOperator ident FixityTable { table, operators } = FixityTable
  { table     = table'
  , operators = operators'
  }
 where
  table'     = removeOperatorFromTable ident table
  operators' = HashSet.delete ident operators

removeOperatorFromTable :: Ident -> Table -> Table
removeOperatorFromTable ident =
  map $ filter (\(ident', _, _) -> ident' /= ident)

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
                      pat lhs rhs = Pat.App { Pat.lhs, Pat.rhs }
                      expr lhs rhs = Expr.App { Expr.lhs, Expr.rhs }
                  in  [ ( Ident.Ident ""
                        , E.InfixL (pat <$ separator)
                        , E.InfixL (expr <$ separator)
                        )
                      ]
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

infixExprOperator :: Precedence -> Associativity -> Text -> TableEntry
infixExprOperator precedence operator name =
  (Ident.Ident name, operator (pat <$ separator), operator (expr <$ separator))
 where
  separator = token_ (Token.Alphanumeric name) <|> token_ (Token.Symbolic name)

  expr lhs rhs = Expr.InfixApp { Expr.lhs
                               , Expr.op         = Ident.Ident name
                               , Expr.precedence
                               , Expr.rhs
                               }

  pat lhs rhs = Pat.InfixApp { Pat.lhs
                             , Pat.op         = Ident.Ident name
                             , Pat.precedence
                             , Pat.rhs
                             }
