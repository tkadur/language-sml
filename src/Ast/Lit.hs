module Ast.Lit where

import           Data.Scientific                ( Scientific )

import           Ast.Lit.Character              ( Character )
import           Common.Positive                ( Positive )

data Lit
    = Int Integer
    | Hex Integer
    | Word Positive
    | HexWord Positive
    | Real Scientific
    | Char [Character]
    | String [Character]
    deriving (Eq, Show)
