{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Declaration where

import           Language.Sml.Ast.Decl
import           Language.Sml.Pretty.Internal.Basic

instance Pretty Decl
