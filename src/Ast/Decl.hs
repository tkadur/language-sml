module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Pat                        ( Pat )
import           Ast.Ident.Long                 ( Long )
import           Ast.Ident.Op                   ( Op )
import           Ast.Ident.StructureIdent       ( StructureIdent )
import           Ast.Ident.TyVar                ( TyVar )
import           Ast.Ident.TyCon                ( TyCon )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Typ                        ( Typ )

data Decl
  = Val
    { isRec :: Bool
    , tyvars :: [TyVar]
    , valbinds :: ValBinds
    }
  | Fun
    { tyvars :: [TyVar]
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
    { new :: TyCon
    , old :: Long TyCon
    }
  | Abstype
    { datbinds :: DatBinds
    , withtype :: Maybe TypBinds
    , decl :: Decl
    }
  | Exception
    { exnbinds :: ExnBinds
    }
  | Local
    { decl :: Decl
    , body :: Decl
    }
  | Open (NonEmpty (Long StructureIdent))
    -- For convenience, express sequence chains with a list instead of
    -- nested @Decl@s
  | Sequence [Decl]
  | Infix
    { precedence :: Maybe Int
    , idents :: NonEmpty ValueIdent
    }
  | Infixr
    { precedence :: Maybe Int
    , idents :: NonEmpty ValueIdent
    }
  | Nonfix
    { idents :: NonEmpty ValueIdent
    }
  deriving (Show)

type ValBinds = NonEmpty ValBind

type FunBinds = NonEmpty FunBind

type TypBinds = NonEmpty TypBind

type DatBinds = NonEmpty DatBind

type ExnBinds = NonEmpty ExnBind

data ValBind
  = ValBind
    { lhs :: Pat
    , rhs :: Expr
    }
  deriving (Show)

data FunBind
  = FunBind
    { clauses :: NonEmpty FunClause
    }
  deriving (Show)

data TypBind
  = TypBind
    { tyvars :: [TyVar]
    , tycon :: TyCon
    , typ :: Typ
    }
  deriving (Show)

data DatBind
  = DatBind
    { tyvars :: [TyVar]
    , tycon :: TyCon
    , conbinds :: NonEmpty ConBind
    }
  deriving (Show)

data ConBind
  = ConBind
    { constructor :: Op ValueIdent
    , arg :: Maybe Typ
    }
  deriving (Show)

data ExnBind
  = ExnBind
    { constructor :: Op ValueIdent
    , arg :: Maybe Typ
    }
  | ExnReplication
    { new :: Op ValueIdent
    , old :: Op (Long ValueIdent )
    }
  deriving (Show)

-- | Function declaration clause
data FunClause
  = InfixClause
    { lhs :: Pat
    , infixName :: ValueIdent
    , rhs :: Pat
    , infixArgs :: [Pat]
    , returnType :: Maybe Typ
    , body :: Expr
    }
  | NonfixClause
    { nonfixName :: Op ValueIdent
    , nonfixArgs :: NonEmpty Pat
    , returnType :: Maybe Typ
    , body :: Expr
    }
  deriving (Show)
