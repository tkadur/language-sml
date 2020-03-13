module Ast.Ident.Ident
  ( Ident(..)
  )
where

newtype Ident = Ident Text
  deriving (Eq, Ord, Hashable, Show)
