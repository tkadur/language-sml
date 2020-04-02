{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.StructureIdent where

import           Ast.Ident.Common               ( Alphanumeric )

-- | A structure name
newtype StructureIdent = StructureIdent Alphanumeric
  deriving (Show)
