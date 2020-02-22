module Ast.Ident where

newtype Ident
    = Ident Text
    deriving (Eq, Hashable, Ord, Show)
