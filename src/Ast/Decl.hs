module Ast.Decl where

import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat

data Decl
    = Val Pat Expr
    deriving (Show)
