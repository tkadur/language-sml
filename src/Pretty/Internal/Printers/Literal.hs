{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Literal where

import           Text.Printf                    ( printf )

import           Ast.Lit
import           Pretty.Internal.Basic

instance Pretty Lit where
  pretty lit = case lit of
    Int i ->
      let (prefix, i') = fixNegative i in pretty $ prefix ++ printf "%d" i'
    Hex i ->
      let (prefix, i') = fixNegative i in pretty $ prefix ++ printf "0x%x" i'
    Word    w -> hcat $ sequence ["0w", pretty w]
    HexWord w -> hcat $ sequence ["0wx", pretty w]
    Real    n -> let (prefix, n') = fixNegative n in pretty $ prefix ++ show n'
    Char    c -> pretty $ toString c
    String  s -> pretty $ toString s

fixNegative :: (Num a, Ord a) => a -> (String, a)
fixNegative n = (prefix, abs n) where prefix = if n < 0 then "~" else ""
