module Language.Sml.Ast.Decl where

import           Language.Sml.Ast.Expr          ( MExpr )
import           Language.Sml.Ast.Pat           ( MPat )
import           Language.Sml.Ast.Ident.Long    ( MLong )
import           Language.Sml.Ast.Ident.Op      ( MOp )
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( MStructureIdent )
import           Language.Sml.Ast.Ident.TyVar   ( MTyVar )
import           Language.Sml.Ast.Ident.TyCon   ( MTyCon )
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( MValueIdent )
import           Language.Sml.Ast.Typ           ( MTyp )
import           Language.Sml.Common.Marked     ( Marked )

type MDecl = Marked Decl

data Decl
  = Val
    { tyvars :: [MTyVar]
    , valbinds :: MValBinds
    }
  | Fun
    { tyvars :: [MTyVar]
    , funbinds :: MFunBinds
    }
  | TypAlias
    { typbinds :: MTypBinds
    }
  | Datatype
    { datbinds :: MDatBinds
    , withtype :: Maybe MTypBinds
    }
  | DatatypeReplication
    { new :: MTyCon
    , old :: MLong MTyCon
    }
  | Abstype
    { datbinds :: MDatBinds
    , withtype :: Maybe MTypBinds
    , decl :: MDecl
    }
  | Exception
    { exnbinds :: MExnBinds
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

type MValBinds = NonEmpty MValBind

type MFunBinds = NonEmpty MFunBind

type MTypBinds = NonEmpty MTypBind

type MDatBinds = NonEmpty MDatBind

type MExnBinds = NonEmpty MExnBind

type MValBind = Marked ValBind

type MFunBind = Marked FunBind

type MTypBind = Marked TypBind

type MDatBind = Marked DatBind

type MExnBind = Marked ExnBind

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
    , returnTyp :: Maybe MTyp
    , body :: MExpr
    }
  | NonfixClause
    { nonfixName :: MOp MValueIdent
    , nonfixArgs :: NonEmpty MPat
    , returnTyp :: Maybe MTyp
    , body :: MExpr
    }
  deriving (Eq, Show)
