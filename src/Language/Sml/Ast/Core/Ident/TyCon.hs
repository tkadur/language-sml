module Language.Sml.Ast.Core.Ident.TyCon where

import           Language.Sml.Ast.Core.Ident.Common
                                                ( Ident )
import           Language.Sml.Common.Marked

type MTyCon = Marked TyCon

-- | A type constructor name
newtype TyCon = TyCon Ident
  deriving (Eq, Show)
