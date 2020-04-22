module Language.Sml.Ast.Toplevel where

import           Language.Sml.Ast.Decl          ( MDecl )

newtype Toplevel = Toplevel MDecl
  deriving (Eq, Show)
