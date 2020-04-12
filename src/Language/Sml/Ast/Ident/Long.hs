module Language.Sml.Ast.Ident.Long where

import           Language.Sml.Ast.Ident.StructureIdent
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
