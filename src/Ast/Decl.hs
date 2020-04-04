module Ast.Decl where

import           Ast.Expr                       ( MExpr )
import           Ast.Pat                        ( MPat )
import           Ast.Ident.Long                 ( MLong )
import           Ast.Ident.Op                   ( MOp )
import           Ast.Ident.StructureIdent       ( MStructureIdent )
import           Ast.Ident.TyVar                ( MTyVar )
import           Ast.Ident.TyCon                ( MTyCon )
import           Ast.Ident.ValueIdent           ( MValueIdent )
import           Ast.Typ                        ( MTyp )
import           Common.Marked                  ( Marked )

type MDecl = Marked Decl

data Decl
  = Val
    { tyvars :: [MTyVar]
    , valbinds :: ValBinds
    }
  | Fun
    { tyvars :: [MTyVar]
    , funbinds :: FunBinds
    }
  | TypAlias
    { typbinds :: TypBinds
    }
  | Datatype
    { datbinds :: DatBinds
    , withtype :: Maybe TypBinds
    }
  | DatatypeReplication
    { new :: MTyCon
    , old :: MLong MTyCon
    }
  | Abstype
    { datbinds :: DatBinds
    , withtype :: Maybe TypBinds
    , decl :: MDecl
    }
  | Exception
    { exnbinds :: ExnBinds
    }
  | Local
    { decl :: MDecl
    , body :: MDecl
    }
  | Open (NonEmpty (MLong MStructureIdent))
    -- For convenience, express sequence chains with a list instead of
    -- nested @MDecl@s
  | Sequence [MDecl]
  | Infix
    { precedence :: Maybe Int
    , idents :: NonEmpty MValueIdent
    }
  | Infixr
    { precedence :: Maybe Int
    , idents :: NonEmpty MValueIdent
    }
  | Nonfix
    { idents :: NonEmpty MValueIdent
    }
  deriving (Eq, Show)

type ValBinds = NonEmpty ValBind

type FunBinds = NonEmpty FunBind

type TypBinds = NonEmpty TypBind

type DatBinds = NonEmpty DatBind

type ExnBinds = NonEmpty ExnBind

data ValBind
  = ValBind
    { isRec :: Bool
    , lhs :: MPat
    , rhs :: MExpr
    }
  deriving (Eq, Show)

newtype FunBind
  = FunBind
    { clauses :: NonEmpty FunClause
    }
  deriving (Eq, Show)

data TypBind
  = TypBind
    { tyvars :: [MTyVar]
    , tycon :: MTyCon
    , typ :: MTyp
    }
  deriving (Eq, Show)

data DatBind
  = DatBind
    { tyvars :: [MTyVar]
    , tycon :: MTyCon
    , conbinds :: NonEmpty ConBind
    }
  deriving (Eq, Show)

data ConBind
  = ConBind
    { constructor :: MOp MValueIdent
    , arg :: Maybe MTyp
    }
  deriving (Eq, Show)

data ExnBind
  = ExnBind
    { constructor :: MOp MValueIdent
    , arg :: Maybe MTyp
    }
  | ExnReplication
    { new :: MOp MValueIdent
    , old :: MOp (MLong MValueIdent )
    }
  deriving (Eq, Show)

-- | Function declaration clause
data FunClause
  = InfixClause
    { lhs :: MPat
    , infixName :: MValueIdent
    , rhs :: MPat
    , infixArgs :: [MPat]
    , returnType :: Maybe MTyp
    , body :: MExpr
    }
  | NonfixClause
    { nonfixName :: MOp MValueIdent
    , nonfixArgs :: NonEmpty MPat
    , returnType :: Maybe MTyp
    , body :: MExpr
    }
  deriving (Eq, Show)
