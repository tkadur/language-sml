{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Type where

import qualified Data.List.NonEmpty            as NonEmpty

import           Ast.Associativity
import qualified Ast.Associativity             as Associativity
import           Ast.Typ
import           Pretty.Internal.Basic
import           Pretty.Internal.Printers.Identifier
                                                ( )

instance Pretty Typ where
  pretty = \case
    TyVar tyvar -> do
      resetTypPrecAssoc
      pretty tyvar

    Record rows -> do
      resetTypPrecAssoc
      record $ mapM pretty rows

    TyCon tycon -> do
      resetTypPrecAssoc
      pretty tycon

    App { tycons, args } ->
      let tyconsPretty = sep $ mapM pretty (NonEmpty.toList tycons)
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
      setTypPrecAssoc PrecAssoc { precedence    = tuplePrec
                                , associativity = tupleAssoc
                                , direction     = Associativity.Left
                                }
      typs
        |> NonEmpty.toList
        |> mapM pretty
        |> punctuate " * "
        |> cat
        |> maybeTypParen prevPrecAssoc

    Arrow { lhs, rhs } -> do
      prevPrecAssoc <- getTypPrecAssoc

      setTypPrecAssoc PrecAssoc { precedence    = arrowPrec
                                , associativity = arrowAssoc
                                , direction     = Associativity.Left
                                }
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc


      setTypPrecAssoc PrecAssoc { precedence    = arrowPrec
                                , associativity = arrowAssoc
                                , direction     = Associativity.Right
                                }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      [lhsPretty <+> "->", rhsPretty]
        |> sequence
        |> sep
        |> maybeTypParen prevPrecAssoc

instance Pretty Row where
  pretty Row { label, typ } = pretty label <+> colon <+> align (pretty typ)

appPrec :: Int
appPrec = 3

appAssoc :: Associativity
appAssoc = Associativity.Left

tuplePrec :: Int
tuplePrec = 2

-- | Nonsense dummy value
tupleAssoc :: Associativity
tupleAssoc = Associativity.Left

arrowPrec :: Int
arrowPrec = 1

arrowAssoc :: Associativity
arrowAssoc = Associativity.Right
