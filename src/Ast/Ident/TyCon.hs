module Ast.Ident.TyCon where

import           Ast.Ident.Common               ( Ident )
import           Common.Marked

type MTyCon = Marked TyCon

-- | A type constructor name
newtype TyCon = TyCon Ident
  deriving (Eq, Show)
