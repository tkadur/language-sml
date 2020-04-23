{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Type where

import qualified Data.List.NonEmpty            as NonEmpty

import           Language.Sml.Ast.Associativity
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Typ
import           Language.Sml.Pretty.Internal.Basic
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                                ( )

-- See Expression.hs of explanation of grouping logic

instance Pretty Typ where
  pretty = \case
    TyVar tyvar -> do
      resetTypPrecAssoc
      pretty tyvar

    Record rows -> do
      resetTypPrecAssoc
      record (mapM pretty rows)

    TyCon tycon -> do
      resetTypPrecAssoc
      pretty tycon

    App { tycons, args } ->
      grouped
        $ let tyconsPretty = sep $ mapM pretty (NonEmpty.toList tycons)
              argsPretty   = case args of
                arg :| [] -> do
                  prevPrecAssoc <- getTypPrecAssoc
                  setTypPrecAssoc PrecAssoc { precedence    = appPrec
                                            , associativity = appAssoc
                                            , direction     = Associativity.Left
                                            }
                  maybeTypParen prevPrecAssoc (pretty arg)
                _ -> do
                  resetTypPrecAssoc
                  tupled $ mapM pretty (NonEmpty.toList args)
          in  sep $ sequence [argsPretty, tyconsPretty]

    Tuple typs -> do
      prevPrecAssoc <- getTypPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = tuplePrec
                                   , associativity = tupleAssoc
                                   , direction     = Associativity.Left
                                   }

      setTypPrecAssoc newPrecAssoc
      let res =
            typs
              |> NonEmpty.toList
              |> mapM ((setTypPrecAssoc newPrecAssoc >>) . pretty)
              |> punctuate " *"
              |> vsep
              |> maybeTypParen prevPrecAssoc
      groupedIf prevPrecAssoc (== tuplePrec) res

    Arrow { lhs, rhs } -> do
      prevPrecAssoc <- getTypPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = arrowPrec
                                   , associativity = arrowAssoc
                                   , direction     = Associativity.Left
                                   }

      setTypPrecAssoc newPrecAssoc
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc

      setTypPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      setTypPrecAssoc newPrecAssoc
      let res =
            [lhsPretty <+> "->", rhsPretty]
              |> sequence
              |> vsep
              |> maybeTypParen prevPrecAssoc
      groupedIf prevPrecAssoc (== tuplePrec) res

instance Pretty Row where
  pretty Row { label, typ } =
    pretty label <+> colon <+> grouped (align $ pretty typ)

appPrec :: Int
appPrec = 3

appAssoc :: Associativity
appAssoc = Associativity.Left

tuplePrec :: Int
tuplePrec = 2

-- Nonsense placeholder value
tupleAssoc :: Associativity
tupleAssoc = Associativity.Right

arrowPrec :: Int
arrowPrec = 1

arrowAssoc :: Associativity
arrowAssoc = Associativity.Right
