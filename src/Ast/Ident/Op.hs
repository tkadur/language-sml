module Ast.Ident.Op where

-- | Possibly op-qualified identifier
data Op ident
  = Ident ident
  | Op ident
  deriving (Eq, Show)
