module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Pat                        ( Pat )
import           Ast.Ident.Ident                ( Ident )

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
