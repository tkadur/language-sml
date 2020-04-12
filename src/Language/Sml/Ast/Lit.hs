module Language.Sml.Ast.Lit where

import           Data.Scientific                ( Scientific )

import           Language.Sml.Ast.Lit.Character ( Character )
import           Language.Sml.Common.Positive   ( Positive )

data Lit
    = Int Integer
    | Hex Integer
    | Word Positive
    | HexWord Positive
    | Real Scientific
    | Char [Character]
    | String [Character]
    deriving (Eq, Show)
