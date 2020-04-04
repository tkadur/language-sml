{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.TyVar where

import           Ast.Ident.Common               ( Ident )

import           Common.Marked                  ( Marked )
import           Common.Positive                ( Positive )

type MTyVar = Marked TyVar

-- | A type variable name
data TyVar
  = TyVar
    { ident :: Ident
    , leadingPrimes :: Positive
    }
  deriving (Eq, Show)
