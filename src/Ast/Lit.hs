module Ast.Lit where

import           Data.Scientific                ( Scientific )

data Lit
    = Int Integer
    | Hex Integer
    | Word Integer
    | HexWord Integer
    | Real Scientific
    | String Text
    deriving (Show)
