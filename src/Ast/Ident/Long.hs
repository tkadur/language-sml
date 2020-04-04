module Ast.Ident.Long where

import           Ast.Ident.StructureIdent       ( MStructureIdent )
import           Common.Marked                  ( Marked )

type MLong ident = Marked (Long ident)

-- | An identifier, possibly qualified
data Long ident
  = Long
    { qualifiers :: [MStructureIdent]
    , ident :: ident
    }
  deriving (Eq, Functor, Show)
