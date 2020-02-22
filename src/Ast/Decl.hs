module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Ident                      ( Ident )
import           Ast.Pat                        ( Pat )

data Decl
    = Infix
        { precedence :: Int
        , ident :: Ident
        }
    | Infixr
        { precedence :: Int
        , ident :: Ident
        }
    | Val
        { lhs :: Pat
        , rhs :: Expr
        }
    deriving (Show)
