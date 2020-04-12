{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Pattern where

import           Language.Sml.Ast.Associativity
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Pat
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Pretty.Internal.Basic
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Literal
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Type
                                                ( )

instance Pretty Pat where
  pretty = \case
    Wild         -> "_"
    Lit    lit   -> pretty lit
    Ident  ident -> pretty ident
    Record rows  -> record $ mapM (grouped . pretty) rows
    Tuple  pats  -> tupled $ mapM (grouped . pretty) pats
    List   pats  -> list $ mapM (grouped . pretty) pats

    Constructed { constructor, arg } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = appPrec
                                 , associativity = appAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprParen
        prevPrecAssoc
        (pretty constructor <> line <> hang (grouped $ pretty arg))

    InfixConstructed { lhs, op, precedence, associativity, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence
                                   , associativity
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- grouped (pretty lhs)
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- grouped (pretty rhs)
      let rhsPretty = return rhsDoc

      setTypPrecAssoc newPrecAssoc
      [lhsPretty, space, pretty op, line, rhsPretty]
        |> sequence
        |> hcat
        |> align
        |> maybeTypParen prevPrecAssoc

    Annot { pat, typ } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = annotPrec
                                 , associativity = annotAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen
        prevPrecAssoc
        (grouped (pretty pat) <+> colon <+> align (grouped $ pretty typ))

    As { ident, annot, as } -> undefined

instance Pretty Row where
  pretty = \case
    RowWild -> "..."
    Row { label, pat } -> pretty label <+> equals <+> pretty pat
    RowPun { ident, annot, as } -> undefined

appPrec :: Int
appPrec = 10

appAssoc :: Associativity
appAssoc = Associativity.Left

annotPrec :: Int
annotPrec = -1

annotAssoc :: Associativity
annotAssoc = Associativity.Right
