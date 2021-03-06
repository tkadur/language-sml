module Language.Sml.Ast.Ident.StructureIdent where

import           Language.Sml.Ast.Ident.Common  ( Alphanumeric )
import           Language.Sml.Common.Marked     ( Marked )

type MStructureIdent = Marked StructureIdent

-- | A structure name
newtype StructureIdent = StructureIdent Alphanumeric
  deriving (Eq, Show)
