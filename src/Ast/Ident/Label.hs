{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.Label where

import           Ast.Ident.Common               ( Ident )
import           Common.Marked                  ( Marked )
import           Common.Positive                ( Positive )

type MLabel = Marked Label

-- | A record label
data Label
  = Ident Ident
  | Numeric Positive
  deriving (Eq, Show)
