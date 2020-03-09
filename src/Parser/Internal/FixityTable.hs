module Parser.Internal.FixityTable
  ( FixityTable()
  , Associativity
  , Precedence
  , Operators
  , Infixable(..)
  , IdentParsers(..)
  , addOperator
  , removeOperator
  , basisFixityTable
  , makeParser
  , operators
  )
where

import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import           Relude.Unsafe                  ( (!!) )
import qualified Text.Megaparsec               as M
import qualified Text.Show

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import qualified Ast.Ident.Ident               as Ident
import           Parser.Internal.Basic

type Associativity = forall a . Parser (a -> a -> a) -> E.Operator Parser a

type Precedence = Int

data IdentParsers = IdentParsers
  { alphanumeric :: Parser (Ident.Tagged 'Ident.Alphanumeric)
  , symbolic :: Parser (Ident.Tagged 'Ident.Symbolic)
  }

data TableEntry = TableEntry
  { operator :: Ident.Untagged
  , parsers :: IdentParsers -> (E.Operator Parser Pat, E.Operator Parser Expr)
  }

type Table = [[TableEntry]]

type Operators = HashSet Ident.Untagged

data FixityTable = FixityTable
  { table :: Table
  , operators :: Operators
  }

instance Show FixityTable where
  show FixityTable { table } =
    (  table
    |> liftTable (\TableEntry { operator } -> operator)
    |> map (\idents -> show idents <> "\n")
    |> concat
    )

data Infixable a where
  Pat ::Infixable Pat
  Expr ::Infixable Expr

makeParser :: IdentParsers -> Infixable a -> Parser a -> FixityTable -> Parser a
makeParser identParsers infixable parser FixityTable { table } =
  table
    |> getTable identParsers infixable
    -- We store the table in ascending precedence order,
    -- but E.makeExprParser expects it in descending order
    |> reverse
    |> E.makeExprParser parser

getTable :: IdentParsers -> Infixable a -> Table -> [[E.Operator Parser a]]
getTable identParsers infixable = case infixable of
  Pat  -> getPatTable
  Expr -> getExprTable
 where
  getPatTable :: Table -> [[E.Operator Parser Pat]]
  getPatTable =
    -- Patterns cannot contain infix "="
    removeOperatorFromTable (Ident.ident "=")
      >>> (liftTable $ \TableEntry { parsers } ->
            let (patTable, _) = parsers identParsers in patTable
          )

  getExprTable :: Table -> [[E.Operator Parser Expr]]
  getExprTable = liftTable $ \TableEntry { parsers } ->
    let (_, exprTable) = parsers identParsers in exprTable

-- | Lifts a function over `TableEntry`s to a function over `Table`s
liftTable :: (TableEntry -> a) -> (Table -> [[a]])
liftTable = fmap . fmap

addOperator :: Ident.Untagged
            -> Associativity
            -> Precedence
            -> FixityTable
            -> FixityTable
addOperator ident associativity precedence FixityTable { table, operators } =
  FixityTable { table = table', operators = operators' }
 where
  table' =
    table
      |> removeOperatorFromTable ident
      -- Add ident to the operator table
      |> update precedence (makeTableEntry precedence associativity ident :)

  operators' = HashSet.insert ident operators

removeOperator :: Ident.Untagged -> FixityTable -> FixityTable
removeOperator ident FixityTable { table, operators } = FixityTable
  { table     = table'
  , operators = operators'
  }
 where
  table'     = removeOperatorFromTable ident table
  operators' = HashSet.delete ident operators

removeOperatorFromTable :: Ident.Untagged -> Table -> Table
removeOperatorFromTable operator' =
  map $ filter (\TableEntry { operator } -> operator /= operator')

basisFixityTable :: FixityTable
basisFixityTable = FixityTable
  { table     =
    [ makeTableEntry 0 E.InfixL <$> (basisOperators !! 0)
    , []
    , []
    , makeTableEntry 3 E.InfixL <$> (basisOperators !! 3)
    , makeTableEntry 4 E.InfixL <$> (basisOperators !! 4)
    , makeTableEntry 5 E.InfixR <$> (basisOperators !! 5)
    , makeTableEntry 6 E.InfixL <$> (basisOperators !! 6)
    , makeTableEntry 7 E.InfixL <$> (basisOperators !! 7)
    , []
    , []
    -- Application
    , let
        separator = nothing
        pat lhs rhs = Pat.App { Pat.lhs, Pat.rhs }
        expr lhs rhs = Expr.App { Expr.lhs, Expr.rhs }
      in
        [ TableEntry
            { operator = Ident.ident ""
            , parsers  = const
              (E.InfixL (pat <$ separator), E.InfixL (expr <$ separator))
            }
        ]
    ]
  , operators = HashSet.fromList $ concat basisOperators
  }
 where
  basisOperators = (map . map $ Ident.ident)
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

makeTableEntry :: Precedence -> Associativity -> Ident.Untagged -> TableEntry
makeTableEntry precedence associativity op = TableEntry { operator = op
                                                        , parsers
                                                        }
 where
  parsers IdentParsers { alphanumeric, symbolic } =
    let separator :: Parser ()
        separator = do
          _ <- symbol $ Ident.name op
          -- Separators may not be followed by more characters of the same type
          case Ident.identType op of
            Ident.Alphanumeric -> M.notFollowedBy alphanumeric
            Ident.Symbolic     -> M.notFollowedBy symbolic
    in  (associativity (pat <$ separator), associativity (expr <$ separator))

  expr lhs rhs = Expr.InfixApp { Expr.lhs, Expr.op, Expr.precedence, Expr.rhs }

  pat lhs rhs = Pat.InfixApp { Pat.lhs, Pat.op, Pat.precedence, Pat.rhs }
