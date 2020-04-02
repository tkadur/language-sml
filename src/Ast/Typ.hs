module Ast.Typ where

import           Ast.Ident.Label                ( Label )
import           Ast.Ident.Long                 ( Long )
import           Ast.Ident.TyCon                ( TyCon )
import           Ast.Ident.TyVar                ( TyVar )

data Typ
  = TyVar TyVar
  | Record [Row]
  | App
    { tycon :: Long TyCon
    , args :: [Typ]
    }
  | Tuple [Typ]
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
