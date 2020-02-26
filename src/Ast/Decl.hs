module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Ident                      ( Ident )
import           Ast.Pat                        ( Pat )

data Decl
    = Infix
        { precedence :: Maybe Int
        , ident :: Ident
        }
    | Infixr
        { precedence :: Maybe Int
        , ident :: Ident
        }
    | Nonfix
        { ident :: Ident
        }
    | Val
        { lhs :: Pat
        , rhs :: Expr
        }
    deriving (Show)
