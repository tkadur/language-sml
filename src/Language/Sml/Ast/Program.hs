module Language.Sml.Ast.Program where

import           Language.Sml.Ast.Core.Decl     ( MDecl )

newtype Program = Program MDecl
  deriving (Eq, Show)
