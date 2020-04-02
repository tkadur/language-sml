module Ast.Expr where

import           Ast.Ident.Label                ( Label )
import           Ast.Ident.Long                 ( Long )
import           Ast.Ident.Op                   ( Op )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import {-# SOURCE #-} Ast.Decl                  ( Decl )
import           Ast.Lit                        ( Lit )
import           Ast.Pat                        ( Pat )
import           Ast.Typ                        ( Typ )

data Expr
  = Lit Lit
  | Ident (Op (Long ValueIdent))
  | Record [Row]
  | RecordSelector Label
  | Tuple [Expr]
  | List [Expr]
  | Sequence [Expr]
  | Let
    { decl :: Decl
    , exprs :: NonEmpty Expr
    }
    -- For convenience, we directly express application chains instead of
    -- using nested @App@s.
  | App
    { function :: Expr
    , args :: NonEmpty Expr
    }
  | InfixApp
    { lhs :: Expr
    , op :: ValueIdent
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
