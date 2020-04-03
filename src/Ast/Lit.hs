module Ast.Lit where

import           Data.Scientific                ( Scientific )

import           Ast.Lit.Character              ( Character )

data Lit
    = Int Integer
    | Hex Integer
    | Word Integer
    | HexWord Integer
    | Real Scientific
    | Char [Character]
    | String [Character]
    deriving (Eq, Show)
