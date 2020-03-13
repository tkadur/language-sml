module Ast.Expr where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )
import           Ast.Pat                        ( Pat )

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
  | Tuple [Expr]
  | List [Expr]
  | Fn
    { match :: Match
    }
  | Case
    { expr :: Expr
    , match :: Match
    }
  deriving (Show)

type Match = NonEmpty MatchArm

data MatchArm
  = MatchArm
    { lhs :: Pat
    , rhs :: Expr
    }
  deriving (Show)
