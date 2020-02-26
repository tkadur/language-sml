module Ast.Expr where

import           Ast.Ident                      ( Ident )
import           Ast.Lit                        ( Lit )
import           Parser.Marked                  ( Marked )

data Expr
  = Lit Lit
  | Var Ident
  | App
    { lhs :: Expr
    , rhs :: Expr
    }
  | InfixApp
    { lhs :: Expr
    , op :: Ident
    , precedence :: Int
    , rhs :: Expr
    }
  deriving (Show)
