{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Identifier where

import qualified Data.Text                     as Text

import           Ast.Ident.Label                ( Label )
import qualified Ast.Ident.Label               as Label
import           Ast.Ident.Long                 ( Long )
import qualified Ast.Ident.Long                as Long
import           Ast.Ident.Op                   ( Op )
import qualified Ast.Ident.Op                  as Op
import           Ast.Ident.StructureIdent       ( StructureIdent )
import qualified Ast.Ident.StructureIdent      as StructureIdent
import           Ast.Ident.TyCon                ( TyCon )
import qualified Ast.Ident.TyCon               as TyCon
import           Ast.Ident.TyVar                ( TyVar )
import qualified Ast.Ident.TyVar               as TyVar
import           Ast.Ident.ValueIdent           ( ValueIdent )
import qualified Ast.Ident.ValueIdent          as ValueIdent
import qualified Common.Positive               as Positive
import           Pretty.Internal.Basic

instance (Pretty ident) => Pretty (Long ident) where
  pretty Long.Long {..} =
    fillCat
      . punctuate dot
      . sequence
      $ (map pretty qualifiers ++ [pretty ident])

instance (Pretty ident) => Pretty (Op ident) where
  pretty = \case
    Op.Ident x -> pretty x
    Op.Op    x -> do
      op <- startsWith ("op" :: Text)
      sep . sequence $ [op, hang 0 $ pretty x]

instance Pretty ValueIdent where
  pretty (ValueIdent.ValueIdent x) = pretty x

instance Pretty StructureIdent where
  pretty (StructureIdent.StructureIdent x) = pretty x

instance Pretty TyCon where
  pretty (TyCon.TyCon x) = pretty x

instance Pretty TyVar where
  pretty TyVar.TyVar {..} = do
    primes <- startsWith
      (Text.replicate (Positive.unPositive leadingPrimes) "'")
    identifier <- endsWith ident
    hcat . sequence $ [primes, identifier]

instance Pretty Label where
  pretty = \case
    Label.Ident   x -> pretty x
    Label.Numeric n -> pretty n
