module Ast.Expr where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.Label                ( Label )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )
import           Ast.Pat                        ( Pat )
import           Ast.Typ                        ( Typ )

data Expr
  = Lit Lit
  | Var ValueIdent
  | Record [Row]
  | RecordSelector Label
  | Tuple [Expr]
  | Sequence [Expr]
  -- TODO(tkadur) let...in...end
  | List [Expr]
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
  | Annot
    { expr :: Expr
    , typ :: Typ
    }
  | Andalso
    { lhs :: Expr
    , rhs :: Expr
    }
  | Orelse
    { lhs :: Expr
    , rhs :: Expr
    }
  | Handle
    { expr :: Expr
    , match :: Match
    }
  | Raise Expr
  | If
    { cond :: Expr
    , ifExpr :: Expr
    , thenExpr :: Expr
    }
  | While
    { cond :: Expr
    , body :: Expr
    }
  | Case
    { expr :: Expr
    , match :: Match
    }
  | Fn
    { match :: Match
    }
  deriving (Show)

type Match = NonEmpty MatchArm

data MatchArm
  = MatchArm
    { lhs :: Pat
    , rhs :: Expr
    }
  deriving (Show)

data Row
  = Row
    { label :: Label
    , expr :: Expr
    }
  deriving (Show)
