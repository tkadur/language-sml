{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Pattern where

import           Language.Sml.Ast.Associativity
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Core.Pat
import           Language.Sml.Pretty.Internal.Basic
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Literal
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Type
                                                ( )

-- See Expression.hs for explanation of grouping logic.

instance Pretty Pat where
  pretty = \case
    Wild -> do
      resetExprPrecAssoc
      "_"
    Lit lit -> do
      resetExprPrecAssoc
      pretty lit
    Ident ident -> do
      resetExprPrecAssoc
      pretty ident
    Record rows -> do
      resetExprPrecAssoc
      record $ mapM pretty rows
    Tuple pats -> do
      resetExprPrecAssoc
      tupled $ mapM (grouped . pretty) pats
    List pats -> do
      resetExprPrecAssoc
      list $ mapM (grouped . pretty) pats

    Constructed { constructor, arg } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = appPrec
                                 , associativity = appAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprParen
        prevPrecAssoc
        (pretty constructor <> line <> grouped (hang $ pretty arg))

    InfixConstructed { lhs, op, precedence, associativity, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence
                                   , associativity
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      setTypPrecAssoc newPrecAssoc

      let res =
            [lhsPretty, space, pretty op, line, rhsPretty]
              |> sequence
              |> hcat
              |> align
              |> maybeTypParen prevPrecAssoc
      groupedIf prevPrecAssoc (`elem` [0 .. 9]) res

    Annot { pat, typ } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = annotPrec
                                 , associativity = annotAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen
        prevPrecAssoc
        (grouped (pretty pat) <+> colon <+> grouped (align $ pretty typ))

    As { ident, annot, as } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = asPrec
                                 , associativity = asAssoc
                                 , direction     = Associativity.Right
                                 }

      let annotPretty = case annot of
            Nothing  -> emptyDoc
            Just typ -> space <> colon <+> align (pretty typ)

      maybeExprParen
        prevPrecAssoc
        (pretty ident <> annotPretty <+> "as" <+> grouped (align $ pretty as))

instance Pretty Row where
  pretty = \case
    RowWild -> "..."
    Row { label, pat } -> pretty label <+> equals <+> grouped (pretty pat)
    RowPun { ident, annot, as } ->
      let annotPretty = case annot of
            Nothing  -> emptyDoc
            Just typ -> space <> colon <+> grouped (align $ pretty typ)
          asPretty = case as of
            Nothing  -> emptyDoc
            Just pat -> space <> "as" <+> grouped (align $ pretty pat)
      in  pretty ident <> annotPretty <> asPretty

appPrec :: Int
appPrec = 10

appAssoc :: Associativity
appAssoc = Associativity.Left

annotPrec :: Int
annotPrec = -1

annotAssoc :: Associativity
annotAssoc = Associativity.Right

asPrec :: Int
asPrec = -2

asAssoc :: Associativity
asAssoc = Associativity.Left
