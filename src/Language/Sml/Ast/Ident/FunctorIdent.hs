module Language.Sml.Ast.Ident.FunctorIdent where

import           Language.Sml.Ast.Ident.Common  ( Alphanumeric )
import           Language.Sml.Common.Marked     ( Marked )

type MFunctorIdent = Marked FunctorIdent

-- | A structure name
newtype FunctorIdent = FunctorIdent Alphanumeric
  deriving (Eq, Show)
