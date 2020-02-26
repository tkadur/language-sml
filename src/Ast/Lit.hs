module Ast.Lit where

data Lit
    = Dec Integer
    | Hex Integer
    | DecWord Integer
    | HexWord Integer
    | String Text
    deriving (Show)
