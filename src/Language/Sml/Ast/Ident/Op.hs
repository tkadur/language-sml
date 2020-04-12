module Language.Sml.Ast.Ident.Op where

import           Language.Sml.Common.Marked     ( Marked )

type MOp ident = Marked (Op ident)

-- | Possibly op-qualified identifier
data Op ident
  = Ident ident
  | Op ident
  deriving (Eq, Functor, Show)
