{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.TyCon where

import           Ast.Ident.Common               ( Ident )

-- | A type constructor name
newtype TyCon = TyCon Ident
  deriving (Eq, Show)
