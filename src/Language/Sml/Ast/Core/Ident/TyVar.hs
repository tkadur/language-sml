module Language.Sml.Ast.Core.Ident.TyVar where

import           Language.Sml.Ast.Core.Ident.Common
                                                ( Ident )

import           Language.Sml.Common.Marked     ( Marked )
import           Language.Sml.Common.Positive   ( Positive )

type MTyVar = Marked TyVar

-- | A type variable name
data TyVar
  = TyVar
    { ident :: Ident
    , leadingPrimes :: Positive
    }
  deriving (Eq, Show)
