module Language.Sml.Ast.Expr where

import           Language.Sml.Ast.Associativity ( Associativity )
import           Language.Sml.Ast.Ident.Label   ( MLabel )
import           Language.Sml.Ast.Ident.Long    ( MLong )
import           Language.Sml.Ast.Ident.Op      ( MOp )
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( MValueIdent )
import {-# SOURCE #-} Language.Sml.Ast.Decl     ( MDecl )
import           Language.Sml.Ast.Lit           ( Lit )
import           Language.Sml.Ast.Pat           ( MPat )
import           Language.Sml.Ast.Typ           ( MTyp )
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
