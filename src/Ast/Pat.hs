module Ast.Pat where

import           Ast.Ident                      ( Ident )
import           Ast.Lit                        ( Lit )

data Pat
    = Lit Lit
    | Var Ident
    deriving (Show)
