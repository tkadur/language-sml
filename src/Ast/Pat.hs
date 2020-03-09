module Ast.Pat where

import qualified Ast.Ident.Ident               as Ident
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
    , op :: Ident.Untagged
    , precedence :: Int
    , rhs :: Pat
    }
  | Tuple [Pat]
  | List [Pat]
  deriving (Show)
