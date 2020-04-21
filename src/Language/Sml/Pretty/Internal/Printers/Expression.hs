{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Expression where

import qualified Data.List.NonEmpty            as NonEmpty

import           Language.Sml.Ast.Associativity
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Expr
import           Language.Sml.Pretty.Internal.Basic
import {-# SOURCE #-} Language.Sml.Pretty.Internal.Printers.Declaration
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Literal
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Pattern
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Type
                                                ( )

instance Pretty Expr where
  pretty = grouped . \case
    Lit      lit   -> pretty lit
    Ident    ident -> pretty ident
    Record   rows  -> record (mapM pretty rows)
    RecordSelector label -> startsWith "#" <> pretty label
    Tuple    exprs -> tupled (mapM pretty exprs)
    List     exprs -> list (mapM pretty exprs)
    Sequence exprs -> parenSequenced (mapM pretty $ NonEmpty.toList exprs)

    Let { decl, exprs } ->
      [ startsWith "let"
        , nest (line <> pretty decl)
        , line
        , "in"
        , nest
          (line <> (grouped . sequenced . mapM pretty $ NonEmpty.toList exprs))
        , line
        , "end"
        ]
        |> sequence
        |> hcat

    App { lhs, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = appPrec
                                   , associativity = appAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      maybeExprParen prevPrecAssoc (lhsPretty <> nest (line <> rhsPretty))

    InfixApp { lhs, op, precedence, associativity, rhs } -> do
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

      setExprPrecAssoc newPrecAssoc

      [lhsPretty <+> pretty op, rhsPretty]
        |> sequence
        |> sep
        |> maybeExprParen prevPrecAssoc

    Annot { expr, typ } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = annotPrec
                                 , associativity = annotAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen prevPrecAssoc
                     (pretty expr <+> colon <+> align (pretty typ))

    Andalso { lhs, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = andalsoPrec
                                   , associativity = andalsoAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      [lhsPretty, "andalso", rhsPretty]
        |> sequence
        |> sep
        |> maybeExprParen prevPrecAssoc

    Orelse { lhs, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = orelsePrec
                                   , associativity = orelseAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- pretty lhs
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- pretty rhs
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      [lhsPretty, "orelse", rhsPretty]
        |> sequence
        |> sep
        |> maybeExprParen prevPrecAssoc

    Handle { expr, match } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = handlePrec
                                 , associativity = handleAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen prevPrecAssoc (pretty expr <+> "handle" <+> pretty match)

    Raise expr -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = raisePrec
                                 , associativity = raiseAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprParen prevPrecAssoc (startsWith "raise" <+> pretty expr)
    If { cond, ifExpr, elseExpr } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = ifPrec
                                   , associativity = ifAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      condDoc <- pretty cond
      let condPretty = return condDoc

      setExprPrecAssoc newPrecAssoc
      ifExprDoc <- pretty ifExpr
      let ifExprPretty = return ifExprDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      elseExprDoc <- pretty elseExpr
      let elseExprPretty = return elseExprDoc

      setExprPrecAssoc newPrecAssoc
      [ startsWith "if "
        , align condPretty
        , " then"
        , nest (line <> ifExprPretty)
        , line
        , "else"
        , nest (line <> elseExprPretty)
        ]
        |> sequence
        |> hcat
        |> maybeExprParen prevPrecAssoc

    While { cond, body } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = whilePrec
                                   , associativity = whileAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      condDoc <- pretty cond
      let condPretty = return condDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      bodyDoc <- pretty body
      let bodyPretty = return bodyDoc

      setExprPrecAssoc newPrecAssoc
      [startsWith "while ", align condPretty, " do", nest (line <> bodyPretty)]
        |> sequence
        |> hcat
        |> maybeExprParen prevPrecAssoc


    Case { expr, match } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = casePrec
                                 , associativity = caseAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprParen
        prevPrecAssoc
        (startsWith "case" <+> pretty expr <+> "of" <> nest
          (hardline <> pretty match)
        )
    Fn { match } -> do
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = fnPrec
                                 , associativity = fnAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprParen prevPrecAssoc (startsWith "fn" <+> align (pretty match))

instance Pretty Row where
  pretty Row { label, expr } = pretty label <+> equals <+> align (pretty expr)

instance Pretty Match where
  pretty arms =
    arms
      |> NonEmpty.toList
      |> mapM pretty
      |> punctuate' separator
      |> vhard
      |> hangBy (-2)
    where separator = "| "

instance Pretty MatchArm where
  pretty MatchArm { lhs, rhs } =
    pretty lhs <+> "=>" <> softline <> (nest . nest $ pretty rhs)

appPrec :: Int
appPrec = 10

appAssoc :: Associativity
appAssoc = Associativity.Left

annotPrec :: Int
annotPrec = -1

annotAssoc :: Associativity
annotAssoc = Associativity.Right

andalsoPrec :: Int
andalsoPrec = -2

andalsoAssoc :: Associativity
andalsoAssoc = Associativity.Right

orelsePrec :: Int
orelsePrec = -3

orelseAssoc :: Associativity
orelseAssoc = Associativity.Right

handlePrec :: Int
handlePrec = -4

handleAssoc :: Associativity
handleAssoc = Associativity.Right

raisePrec :: Int
raisePrec = -5

raiseAssoc :: Associativity
raiseAssoc = Associativity.Right

ifPrec :: Int
ifPrec = -6

ifAssoc :: Associativity
ifAssoc = Associativity.Right

whilePrec :: Int
whilePrec = -7

whileAssoc :: Associativity
whileAssoc = Associativity.Right

casePrec :: Int
casePrec = -8

caseAssoc :: Associativity
caseAssoc = Associativity.Right

fnPrec :: Int
fnPrec = -9

fnAssoc :: Associativity
fnAssoc = Associativity.Right
