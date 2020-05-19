{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Identifier where

import qualified Data.Text                     as Text

import           Language.Sml.Ast.Ident.Label   ( Label )
import qualified Language.Sml.Ast.Ident.Label  as Label
import           Language.Sml.Ast.Ident.Long    ( Long )
import qualified Language.Sml.Ast.Ident.Long   as Long
import           Language.Sml.Ast.Ident.Op      ( Op )
import qualified Language.Sml.Ast.Ident.Op     as Op
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( StructureIdent )
import qualified Language.Sml.Ast.Ident.StructureIdent
                                               as StructureIdent
import           Language.Sml.Ast.Ident.TyCon   ( TyCon )
import qualified Language.Sml.Ast.Ident.TyCon  as TyCon
import           Language.Sml.Ast.Ident.TyVar   ( TyVar )
import qualified Language.Sml.Ast.Ident.TyVar  as TyVar
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( ValueIdent )
import qualified Language.Sml.Ast.Ident.ValueIdent
                                               as ValueIdent
import qualified Language.Sml.Common.Positive  as Positive
import           Language.Sml.Pretty.Internal.Basic

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
      let op = startsWith "op"
      sep . sequence $ [op, align $ pretty x]

instance Pretty ValueIdent where
  pretty (ValueIdent.ValueIdent x) = pretty x

instance Pretty StructureIdent where
  pretty (StructureIdent.StructureIdent x) = pretty x

instance Pretty TyCon where
  pretty (TyCon.TyCon x) = pretty x

instance Pretty TyVar where
  pretty TyVar.TyVar {..} = do
    let primes =
          startsWith (Text.replicate (Positive.unPositive leadingPrimes) "'")
    let identifier = endsWith ident
    hcat . sequence $ [primes, identifier]

instance Pretty Label where
  pretty = \case
    Label.Ident   x -> pretty x
    Label.Numeric n -> pretty n
