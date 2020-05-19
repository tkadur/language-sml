{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Program where

import           Language.Sml.Ast.Program
import           Language.Sml.Pretty.Internal.Basic
import           Language.Sml.Pretty.Internal.Printers.Declaration
                                                ( )

instance Pretty Program where
  pretty (Program decl) = pretty decl
