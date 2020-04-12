module Language.Sml.Ast.Typ where

import           Language.Sml.Ast.Ident.Label   ( MLabel )
import           Language.Sml.Ast.Ident.Long    ( MLong )
import           Language.Sml.Ast.Ident.TyCon   ( MTyCon )
import           Language.Sml.Ast.Ident.TyVar   ( MTyVar )
import           Language.Sml.Common.Marked     ( Marked )

type MTyp = Marked Typ

type MRow = Marked Row

data Typ
  = TyVar MTyVar
  | Record [MRow]
  | TyCon (MLong MTyCon)
  | App
    { tycons :: NonEmpty (MLong MTyCon)
    , args :: NonEmpty MTyp
    }
  | Tuple (NonEmpty MTyp)
  | Arrow
    { lhs :: MTyp
    , rhs :: MTyp
    }
  deriving (Eq, Show)

data Row
  = Row
    { label :: MLabel
    , typ :: MTyp
    }
  deriving (Eq, Show)
