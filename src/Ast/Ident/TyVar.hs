{-# LANGUAGE EmptyDataDeriving #-}

module Ast.Ident.TyVar where

import           Ast.Ident.Common               ( Ident )

import           Common.Positive                ( Positive )

-- | A type variable name
data TyVar
  = TyVar
    { ident :: Ident
    , leadingPrimes :: Positive
    }
  deriving (Eq, Show)
