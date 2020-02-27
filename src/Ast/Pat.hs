module Ast.Pat where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )

data Pat
  = Lit Lit
  | Var ValueIdent
  | App
    { lhs :: Pat
    , rhs :: Pat
    }
  | InfixApp
    { lhs :: Pat
    , op :: Ident
    , precedence :: Int
    , rhs :: Pat
    }
  deriving (Show)
