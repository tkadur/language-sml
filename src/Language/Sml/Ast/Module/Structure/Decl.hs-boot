module Language.Sml.Ast.Module.Structure.Decl where

import           Language.Sml.Common.Marked     ( Marked )

type MDecl = Marked Decl

data Decl

instance Eq Decl

instance Show Decl
