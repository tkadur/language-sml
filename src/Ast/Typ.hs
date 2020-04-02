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
    { tycon :: Long TyCon
    , args :: NonEmpty Typ
    }
  | Tuple (NonEmpty Typ)
  | Arrow
    { lhs :: Typ
    , rhs :: Typ
    }
  deriving (Show)

data Row
  = Row
    { label :: Label
    , typ :: Typ
    }
  deriving (Show)
