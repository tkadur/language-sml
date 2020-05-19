module Language.Sml.Ast.Core.Expr where

import           Language.Sml.Ast.Associativity ( Associativity )
import           Language.Sml.Ast.Core.Ident.Label
                                                ( MLabel )
import           Language.Sml.Ast.Core.Ident.Long
                                                ( MLong )
import           Language.Sml.Ast.Core.Ident.Op ( MOp )
import           Language.Sml.Ast.Core.Ident.ValueIdent
                                                ( MValueIdent )
import {-# SOURCE #-} Language.Sml.Ast.Core.Decl
                                                ( MDecl )
import           Language.Sml.Ast.Core.Lit      ( Lit )
import           Language.Sml.Ast.Core.Pat      ( MPat )
import           Language.Sml.Ast.Core.Typ      ( MTyp )
import           Language.Sml.Common.Marked     ( Marked )

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
