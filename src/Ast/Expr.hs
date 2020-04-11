module Ast.Expr where

import           Ast.Associativity              ( Associativity )
import           Ast.Ident.Label                ( MLabel )
import           Ast.Ident.Long                 ( MLong )
import           Ast.Ident.Op                   ( MOp )
import           Ast.Ident.ValueIdent           ( MValueIdent )
import {-# SOURCE #-} Ast.Decl                  ( MDecl )
import           Ast.Lit                        ( Lit )
import           Ast.Pat                        ( MPat )
import           Ast.Typ                        ( MTyp )
import           Common.Marked                  ( Marked )

type MExpr = Marked Expr

data Expr
  = Lit Lit
  | Ident (MOp (MLong MValueIdent))
  | Record [Row]
  | RecordSelector MLabel
  | Tuple [MExpr]
  | List [MExpr]
  | Sequence (NonEmpty MExpr)
  | Let
    { decl :: MDecl
    , exprs :: NonEmpty MExpr
    }
    -- For convenience, we directly express application chains instead of
    -- using nested @App@s.
  | App
    { lhs :: MExpr
    , rhs :: MExpr
    }
  | InfixApp
    { lhs :: MExpr
    , op :: MValueIdent
    , precedence :: Int
    , associativity :: Associativity
    , rhs :: MExpr
    }
  | Annot
    { expr :: MExpr
    , typ :: MTyp
    }
  | Andalso
    { lhs :: MExpr
    , rhs :: MExpr
    }
  | Orelse
    { lhs :: MExpr
    , rhs :: MExpr
    }
  | Handle
    { expr :: MExpr
    , match :: Match
    }
  | Raise MExpr
  | If
    { cond :: MExpr
    , ifExpr :: MExpr
    , elseExpr :: MExpr
    }
  | While
    { cond :: MExpr
    , body :: MExpr
    }
  | Case
    { expr :: MExpr
    , match :: Match
    }
  | Fn
    { match :: Match
    }
  deriving (Eq, Show)

type Match = NonEmpty MatchArm

data MatchArm
  = MatchArm
    { lhs :: MPat
    , rhs :: MExpr
    }
  deriving (Eq, Show)

data Row
  = Row
    { label :: MLabel
    , expr :: MExpr
    }
  deriving (Eq, Show)
