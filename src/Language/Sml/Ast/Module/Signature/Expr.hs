module Language.Sml.Ast.Module.Signature.Expr where

import           Language.Sml.Ast.Core.Typ      ( MTyp )
import           Language.Sml.Ast.Ident.Long    ( MLong )
import           Language.Sml.Ast.Ident.SignatureIdent
                                                ( MSignatureIdent )
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( MStructureIdent )
import           Language.Sml.Ast.Ident.TyCon   ( MTyCon )
import           Language.Sml.Ast.Ident.TyVar   ( MTyVar )
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( MValueIdent )
import           Language.Sml.Common.Marked     ( Marked )

type MExpr = Marked Expr

data Expr
  = Spec MSpec
  | Ident MSignatureIdent
  | WhereTyp
    { expr :: MExpr
    , tyvars :: [MTyVar]
    , tycon :: MLong MTyCon
    , typ :: MTyp
    }
  deriving (Eq, Show)

type MSpec = Marked Spec

data Spec
  = Val
    { valdescs :: MValDescs
    }
  | Typ
    { typdescs :: MTypDescs
    }
  | EqTyp
    { typdsecs :: MTypDescs
    }
  | Datatype
    { datdescs :: MDatDescs
    }
  | DatatypeReplication
    { new :: MTyCon
    , old :: MLong MTyCon
    }
  | Exception
    { exndescs :: MExnDescs
    }
  | Structure
    { structuredescs :: MStructureDescs
    }
  | Include MExpr
  | Sequence [MSpec]
  | SharingTyp
    { spec :: MSpec
    , tycons :: NonEmpty (MLong MTyCon)
    }
  deriving (Eq, Show)

type MValDescs = NonEmpty MValDesc

type MTypDescs = NonEmpty MTypDesc

type MDatDescs = NonEmpty MDatDesc

type MConDescs = NonEmpty MConDesc

type MExnDescs = NonEmpty MExnDesc

type MStructureDescs = NonEmpty MStructureDesc

type MValDesc = Marked ValDesc

type MTypDesc = Marked TypDesc

type MDatDesc = Marked DatDesc

type MConDesc = Marked ConDesc

type MExnDesc = Marked ExnDesc

type MStructureDesc = Marked StructureDesc

data ValDesc
  = ValDesc
    { ident :: MValueIdent
    , typ :: MTyp
    }
  deriving (Eq, Show)

data TypDesc
  = TypDesc
    { tyvars :: [MTyVar]
    , tycon :: MTyCon
    }
  deriving (Eq, Show)

data DatDesc
  = DatDesc
    { tyvars :: [MTyVar]
    , tycon :: MTyCon
    , condesc :: MConDesc
    }
  deriving (Eq, Show)

data ConDesc
  = ConDesc
    { ident :: MValueIdent
    , typ :: Maybe MTyp
    }
  deriving (Eq, Show)

data ExnDesc
  = ExnDesc
    { ident :: MValueIdent
    , typ :: Maybe MTyp
    }
  deriving (Eq, Show)

data StructureDesc
  = StructureDesc
    { ident :: MStructureIdent
    , sig :: MExpr
    }
  deriving (Eq, Show)
