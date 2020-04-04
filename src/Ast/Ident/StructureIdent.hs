module Ast.Ident.StructureIdent where

import           Ast.Ident.Common               ( Alphanumeric )
import           Common.Marked                  ( Marked )

type MStructureIdent = Marked StructureIdent

-- | A structure name
newtype StructureIdent = StructureIdent Alphanumeric
  deriving (Eq, Show)
