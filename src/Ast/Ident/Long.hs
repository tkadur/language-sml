module Ast.Ident.Long
  ( Long(..)
  )
where

import           Ast.Ident.StructureIdent       ( StructureIdent )

-- | An identifier, possibly qualified
data Long ident
  = Long
    { qualifiers :: [StructureIdent]
    , ident :: ident
    }
  deriving (Show)
