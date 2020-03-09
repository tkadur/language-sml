module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Pat                        ( Pat )
import qualified Ast.Ident.Ident               as Ident

data Decl
    = Infix
        { precedence :: Maybe Int
        , ident :: Ident.Untagged
        }
    | Infixr
        { precedence :: Maybe Int
        , ident :: Ident.Untagged
        }
    | Nonfix
        { ident :: Ident.Untagged
        }
    | Val
        { lhs :: Pat
        , rhs :: Expr
        }
    deriving (Show)
