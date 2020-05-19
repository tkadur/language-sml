module Language.Sml.Ast.Core.Ident.ValueIdent where

import           Language.Sml.Ast.Core.Ident.Common
                                                ( Ident )
import           Language.Sml.Common.Marked     ( Marked )

type MValueIdent = Marked ValueIdent

newtype ValueIdent = ValueIdent Ident
  deriving (Eq, Ord, Hashable, Show)
