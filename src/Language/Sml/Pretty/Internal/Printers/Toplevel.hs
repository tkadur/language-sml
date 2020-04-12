{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Toplevel where

import           Language.Sml.Ast.Toplevel
import           Language.Sml.Pretty.Internal.Basic
import           Language.Sml.Pretty.Internal.Printers.Declaration
                                                ( )

instance Pretty Toplevel where
  pretty (Toplevel decl) = pretty decl
