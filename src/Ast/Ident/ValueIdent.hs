module Ast.Ident.ValueIdent where

import           Ast.Ident.Common               ( Ident )
import           Common.Marked                  ( Marked )

type MValueIdent = Marked ValueIdent

newtype ValueIdent = ValueIdent Ident
  deriving (Eq, Ord, Hashable, Show)
