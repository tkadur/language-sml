module Ast.Decl where

import           Common.Marked                  ( Marked )

type MDecl = Marked Decl

data Decl

instance Eq Decl

instance Show Decl