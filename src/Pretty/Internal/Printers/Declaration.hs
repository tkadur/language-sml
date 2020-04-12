{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Declaration where

import qualified Data.List.NonEmpty            as NonEmpty

import           Ast.Decl
import           Ast.Ident.TyVar                ( MTyVar )
import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Pretty.Internal.Basic

import           Pretty.Internal.Printers.Expression
                                                ( )
import           Pretty.Internal.Printers.Identifier
                                                ( )
import           Pretty.Internal.Printers.Literal
                                                ( )
import           Pretty.Internal.Printers.Pattern
                                                ( )
import           Pretty.Internal.Printers.Type  ( )

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
