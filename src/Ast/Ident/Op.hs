module Ast.Ident.Op where

import           Common.Marked                  ( Marked )

type MOp ident = Marked (Op ident)

-- | Possibly op-qualified identifier
data Op ident
  = Ident ident
  | Op ident
  deriving (Eq, Functor, Show)
