module Ast.Ident.ValueIdent where

import           Ast.Ident.Common               ( Ident )

newtype ValueIdent = ValueIdent Ident
  deriving (Eq, Ord, Hashable, Show)
