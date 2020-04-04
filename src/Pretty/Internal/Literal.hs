{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Literal where

import           Data.Text.Prettyprint.Doc
import           Text.Printf                    ( printf )

import           Ast.Lit
import           Pretty.Internal.Util           ( )

instance Pretty Lit where
  pretty = \case
    Int i ->
      let (prefix, i') = fixNegative i in pretty $ prefix ++ printf "%d" i'
    Hex i ->
      let (prefix, i') = fixNegative i in pretty $ prefix ++ printf "0x%x" i'
    Word    w -> "0w" <> pretty w
    HexWord w -> "0wx" <> pretty w
    Real    n -> let (prefix, n') = fixNegative n in pretty $ prefix ++ show n'
    Char    c -> pretty $ toString c
    String  s -> pretty $ toString s

fixNegative :: (Num a, Ord a) => a -> (String, a)
fixNegative n = (prefix, abs n) where prefix = if n < 0 then "~" else ""
