module Ast.Expr where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )

data Expr
  = Lit Lit
  | Var ValueIdent
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
