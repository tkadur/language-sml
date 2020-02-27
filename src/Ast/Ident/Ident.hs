module Ast.Ident.Ident where

-- | A bare identifier
newtype Ident
    = Ident Text
    deriving (Eq, Hashable, Ord, Show)
