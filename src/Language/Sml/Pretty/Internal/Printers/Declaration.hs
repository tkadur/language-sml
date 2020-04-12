{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Declaration where

import qualified Data.List.NonEmpty            as NonEmpty

import           Language.Sml.Ast.Decl
import           Language.Sml.Ast.Ident.TyVar   ( MTyVar )
import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Pretty.Internal.Basic

import           Language.Sml.Pretty.Internal.Printers.Expression
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Literal
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Pattern
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Type
                                                ( )

instance Pretty Decl where
  pretty = \case
    Val { tyvars, valbinds } ->
      let preamble = case tyvars of
            []      -> startsWith "val"
            [tyvar] -> startsWith "val" <+> pretty tyvar
            _       -> startsWith "val" <+> tupled (mapM pretty tyvars)
      in  preamble <+> binds valbinds

instance Pretty ValBind where
  pretty ValBind { isRec, lhs, rhs } =
    recPretty <> grouped (nest $ pretty lhs) <+> equals <> nest
      (line <> grouped (pretty rhs))
   where
    recPretty = case isRec of
      True  -> startsWith "rec "
      False -> emptyDoc

binds :: (Pretty a, Show a) => NonEmpty (Marked a) -> Doc ann
binds = \case
  firstBind :| [] -> pretty firstBind
  firstBind :| andBinds ->
    let andBindsPretty = map
          (\andBind@Marked.Marked { Marked.startPosition } ->
            pretty
                (Marked.Marked { Marked.value         = "and" :: Text
                               , Marked.startPosition
                               , Marked.endPosition   = startPosition
                               }
                )
              <+> pretty andBind
          )
          andBinds
    in  vhard . sequence $ (pretty firstBind : andBindsPretty)
