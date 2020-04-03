{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.Label where

import           Ast.Ident.Common               ( Ident )
import           Common.Positive                ( Positive )

-- | A record label
data Label
  = Ident Ident
  | Numeric Positive
  deriving (Eq, Show)
