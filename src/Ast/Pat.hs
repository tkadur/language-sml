module Ast.Pat where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )

data Pat
  = Wild
  | Lit Lit
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
  | Tuple [Pat]
  deriving (Show)
