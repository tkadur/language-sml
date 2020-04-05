module Ast.Typ where

import           Ast.Ident.Label                ( MLabel )
import           Ast.Ident.Long                 ( MLong )
import           Ast.Ident.TyCon                ( MTyCon )
import           Ast.Ident.TyVar                ( MTyVar )
import           Common.Marked                  ( Marked )

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
  -- For convenience, we directly express arrow type chains instead of nesting them
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
