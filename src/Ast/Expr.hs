module Ast.Expr where

import           Ast.Ident                      ( Ident )
import           Ast.Lit                        ( Lit )

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
    , rhs :: Expr
    }
  deriving (Show)
