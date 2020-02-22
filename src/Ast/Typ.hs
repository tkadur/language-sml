module Ast.Typ where

import           Ast.Ident                      ( Ident )

data Typ
    = Ident Ident
    deriving (Show)
