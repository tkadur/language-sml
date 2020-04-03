module Ast.Typ where

import           Ast.Ident.Label                ( Label )
import           Ast.Ident.Long                 ( Long )
import           Ast.Ident.TyCon                ( TyCon )
import           Ast.Ident.TyVar                ( TyVar )

data Typ
  = TyVar TyVar
  | Record [Row]
  | TyCon (Long TyCon)
  | App
    { tycons :: NonEmpty (Long TyCon)
    , args :: NonEmpty Typ
    }
  -- For convenience, we directly express product/arrow type chains instead of nesting them
  | Tuple (NonEmpty Typ)
  | Arrow (NonEmpty Typ)
  deriving (Eq, Show)

data Row
  = Row
    { label :: Label
    , typ :: Typ
    }
  deriving (Eq, Show)
