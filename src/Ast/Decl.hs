module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Pat                        ( Pat )
import           Ast.Ident.Ident                ( Ident )

data Decl
    = Infix
        { precedence :: Maybe Int
        , idents :: NonEmpty Ident
        }
    | Infixr
        { precedence :: Maybe Int
        , idents :: NonEmpty Ident
        }
    | Nonfix
        { idents :: NonEmpty Ident
        }
    | Val
        { lhs :: Pat
        , rhs :: Expr
        }
    | ValRec
        { lhs :: Pat
        , rhs :: Expr
        }
    | ValAnd
        { lhs :: Pat
        , rhs :: Expr
        }
    deriving (Show)
