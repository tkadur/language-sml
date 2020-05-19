module Language.Sml.Ast.Core.Ident.Long where

import           Language.Sml.Ast.Core.Ident.StructureIdent
                                                ( MStructureIdent )
import           Language.Sml.Common.Marked     ( Marked )

type MLong ident = Marked (Long ident)

-- | An identifier, possibly qualified
data Long ident
  = Long
    { qualifiers :: [MStructureIdent]
    , ident :: ident
    }
  deriving (Eq, Functor, Show)
