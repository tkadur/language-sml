module Language.Sml.Ast.Core.Ident.Label where

import           Language.Sml.Ast.Core.Ident.Common
                                                ( Ident )
import           Language.Sml.Common.Marked     ( Marked )
import           Language.Sml.Common.Positive   ( Positive )

type MLabel = Marked Label

-- | A record label
data Label
  = Ident Ident
  | Numeric Positive
  deriving (Eq, Show)
