{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Pattern where

import           Ast.Associativity
import qualified Ast.Associativity             as Associativity
import           Ast.Pat
import qualified Common.Marked                 as Marked
import           Pretty.Internal.Basic
import           Pretty.Internal.Printers.Identifier
                                                ( )
import           Pretty.Internal.Printers.Literal
                                                ( )
import           Pretty.Internal.Printers.Type  ( )

instance Pretty Pat where
  pretty = \case
    Wild         -> "_"
    Lit    lit   -> pretty lit
    Ident  ident -> pretty ident
    Record rows  -> record $ mapM pretty rows
    Tuple  pats  -> tupled $ mapM pretty pats
    List   pats  -> list $ mapM pretty pats

    Constructed { constructor, arg } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = appPrec
                                 , associativity = appAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprParen
        prevPrecAssoc
        (pretty constructor <> softlineNest <> align (pretty arg))

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
      [lhsPretty <+> pretty op, rhsPretty]
        |> sequence
        |> sep
        |> maybeTypParen prevPrecAssoc

    Annot { pat, typ } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = annotPrec
                                 , associativity = annotAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen prevPrecAssoc (pretty pat <+> colon <+> align (pretty typ))

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
