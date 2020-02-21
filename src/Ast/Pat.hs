module Ast.Pat where

import           Ast.Ident                      ( Ident )
import qualified Ast.Ident                     as Ident
import           Ast.Lit                        ( Lit )
import qualified Ast.Lit                       as Lit

data Pat
    = Lit Lit
    | Var Ident
    deriving (Show)
