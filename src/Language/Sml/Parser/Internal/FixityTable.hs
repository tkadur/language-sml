module Language.Sml.Parser.Internal.FixityTable
  ( FixityTable()
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

import           Language.Sml.Ast.Associativity ( Associativity )
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Expr          ( MExpr )
import qualified Language.Sml.Ast.Expr         as Expr
import           Language.Sml.Ast.Pat           ( MPat )
import qualified Language.Sml.Ast.Pat          as Pat
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( ValueIdent )
import qualified Language.Sml.Ast.Ident.ValueIdent
                                               as ValueIdent
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

type Precedence = Int

type TableEntry = (ValueIdent, E.Operator Parser MPat, E.Operator Parser MExpr)

type Table = [[TableEntry]]

type Operators = HashSet ValueIdent

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
  Pat ::Infixable MPat
  Expr ::Infixable MExpr

makeParser :: (MonadParser parser)
           => Infixable a
           -> parser a
           -> FixityTable
           -> parser a
makeParser infixable parser FixityTable { table } =
  table
    |> getTable infixable
    -- We store the table in ascending precedence order,
    -- but E.makeExprParser expects it in descending order
    |> reverse
    |> liftOperators
    |> E.makeExprParser parser
 where
  liftOperators :: (MonadParser parser)
                => [[E.Operator Parser a]]
                -> [[E.Operator parser a]]
  liftOperators = fmap . fmap $ \case
    E.InfixN  cmb -> E.InfixN (liftParser cmb)
    E.InfixL  cmb -> E.InfixL (liftParser cmb)
    E.InfixR  cmb -> E.InfixR (liftParser cmb)
    E.Prefix  cmb -> E.Prefix (liftParser cmb)
    E.Postfix cmb -> E.Postfix (liftParser cmb)
    E.TernR   cmb -> E.TernR (liftParser <$> liftParser cmb)

getTable :: Infixable a -> Table -> [[E.Operator Parser a]]
getTable infixable = case infixable of
  Pat  -> getPatTable
  Expr -> getExprTable
 where
  getPatTable :: Table -> [[E.Operator Parser MPat]]
  getPatTable table =
    table
      -- Patterns cannot contain infix "="
      |> removeOperatorFromTable (ValueIdent.ValueIdent "=")
      |> liftTable (\(_, patTable, _) -> patTable)

  getExprTable :: Table -> [[E.Operator Parser MExpr]]
  getExprTable = liftTable $ \(_, _, exprTable) -> exprTable

-- | Lifts a function over `TableEntry`s to a function over `Table`s
liftTable :: (TableEntry -> a) -> (Table -> [[a]])
liftTable = fmap . fmap

addOperators :: NonEmpty ValueIdent
             -> Associativity
             -> Precedence
             -> FixityTable
             -> FixityTable
addOperators idents associativity precedence =
  idents
    |> NonEmpty.map (\ident -> addOperator ident associativity precedence)
    |> foldl' (.) id

addOperator :: ValueIdent
            -> Associativity
            -> Precedence
            -> FixityTable
            -> FixityTable
addOperator ident@(ValueIdent.ValueIdent name) associativity precedence FixityTable { table, operators }
  = FixityTable { table = table', operators = operators' }
 where
  table' =
    table
      |> removeOperatorFromTable ident
      -- Add ident to the operator table
      |> update precedence (infixExprOperator precedence associativity name :)

  operators' = HashSet.insert ident operators

removeOperators :: NonEmpty ValueIdent -> FixityTable -> FixityTable
removeOperators = foldl' (.) id . NonEmpty.map removeOperator

removeOperator :: ValueIdent -> FixityTable -> FixityTable
removeOperator ident FixityTable { table, operators } = FixityTable
  { table     = table'
  , operators = operators'
  }
 where
  table'     = removeOperatorFromTable ident table
  operators' = HashSet.delete ident operators

removeOperatorFromTable :: ValueIdent -> Table -> Table
removeOperatorFromTable ident =
  map $ filter (\(ident', _, _) -> ident' /= ident)

basisFixityTable :: FixityTable
basisFixityTable = FixityTable
  { table     =
    [ infixExprOperator 0 Associativity.Left <$> (basisOperators !! 0)
    , []
    , []
    , infixExprOperator 3 Associativity.Left <$> (basisOperators !! 3)
    , infixExprOperator 4 Associativity.Left <$> (basisOperators !! 4)
    , infixExprOperator 5 Associativity.Right <$> (basisOperators !! 5)
    , infixExprOperator 6 Associativity.Left <$> (basisOperators !! 6)
    , infixExprOperator 7 Associativity.Left <$> (basisOperators !! 7)
    , []
    , []
              -- Application
    , [ let separator = nothing
            -- Flatten application chains when possible
            expr lhs rhs =
              Marked.merge lhs rhs (Expr.App { Expr.lhs, Expr.rhs })
        in  ( ValueIdent.ValueIdent ""
            -- application cannot appear in patterns
            , E.InfixL never
            , E.InfixL (expr <$ separator)
            )
      ]
    ]
  , operators = HashSet.fromList
                  $ map ValueIdent.ValueIdent (concat basisOperators)
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
infixExprOperator precedence associativity name =
  ( ValueIdent.ValueIdent name
    -- Hack to deal with "=" not being allowed in patterns
  , operator $ case name of
    "=" -> never
    _   -> pat <$> separator
  , operator (expr <$> separator)
  )
 where
  -- Hack to deal "="" being a special token
  separator = marked $ case name of
    "=" -> token_ Token.Equal
    _   -> token_ (Token.Alphanumeric name) <|> token_ (Token.Symbolic name)

  expr sep lhs rhs = Marked.merge
    lhs
    rhs
    (Expr.InfixApp { Expr.lhs
                   , Expr.op         = ValueIdent.ValueIdent name <$ sep
                   , Expr.precedence
                   , Expr.associativity
                   , Expr.rhs
                   }
    )

  pat sep lhs rhs = Marked.merge
    lhs
    rhs
    (Pat.InfixConstructed { Pat.lhs
                          , Pat.op         = ValueIdent.ValueIdent name <$ sep
                          , Pat.precedence
                          , Pat.associativity
                          , Pat.rhs
                          }
    )

  operator :: forall a . Parser (a -> a -> a) -> E.Operator Parser a
  operator = case associativity of
    Associativity.Left  -> E.InfixL
    Associativity.Right -> E.InfixR
