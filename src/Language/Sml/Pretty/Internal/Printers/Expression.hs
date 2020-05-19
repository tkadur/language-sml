{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Expression where

import qualified Data.List.NonEmpty            as NonEmpty

import           Language.Sml.Ast.Associativity
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Core.Expr
import qualified Language.Sml.Common.Marked    as Marked
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

-- We want expressions which feel "homogeneous", in a sense, to be grouped together.
-- So `x + y + z` should be grouped as one expression even though there's nesting and in
-- the corresponding AST. So we insert @grouped@ for pretty-printing nested @Expr@s
-- everywhere except for those "homogeneous" cases. In those cases, we insert @grouped@
-- only if the parent expression is of the same form.

-- Only don't group along with pretty when
--  - You want to group a big chunk at a time (see above)
--  - You know the thing being pretty-printed has no groupable newlines

-- TODO(tkadur) Grouping management in general is a massive hack. Do it better.

instance Pretty Expr where
  pretty = \case
    Lit lit -> do
      resetExprPrecAssoc
      pretty lit

    Ident ident -> do
      resetExprPrecAssoc
      pretty ident

    Record rows -> do
      resetExprPrecAssoc
      setPatternMatching False
      record (mapM (grouped . pretty) rows)

    RecordSelector label -> do
      resetExprPrecAssoc
      startsWith "#" <> pretty label

    Tuple exprs -> do
      resetExprPrecAssoc
      setPatternMatching False
      tupled (mapM (grouped . pretty) exprs)

    List exprs -> do
      resetExprPrecAssoc
      setPatternMatching False
      list (mapM (grouped . pretty) exprs)

    Sequence exprs -> do
      resetExprPrecAssoc
      setPatternMatching False
      parenSequenced (mapM (grouped . pretty) $ NonEmpty.toList exprs)

    Let { decl, exprs } -> do
      resetExprPrecAssoc
      setPatternMatching False

      [ startsWith "let"
        , nest (line <> pretty decl)
        , line
        , "in"
        , nest
          (line <> sequenced (mapM (grouped . pretty) $ NonEmpty.toList exprs))
        , line
        , endsWith "end"
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
      lhsDoc <- bracketPatternMatching (pretty lhs)
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- bracketPatternMatching (pretty rhs)
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      let res = maybeExprParen prevPrecAssoc
                               (lhsPretty <> nest (line <> rhsPretty))
      -- Only group together @App@ when not coming from another @App@
      groupedIf prevPrecAssoc (== appPrec) res

    InfixApp { lhs, op, precedence, associativity, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence
                                   , associativity
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- bracketPatternMatching (pretty lhs)
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- bracketPatternMatching (pretty rhs)
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      let res =
            [lhsPretty, pretty op <+> rhsPretty]
              |> sequence
              |> vsep
              |> maybeExprParen prevPrecAssoc
      -- Only group together @InfixApp@ when not coming from another @InfixApp@
      groupedIf prevPrecAssoc (`elem` [0 .. 9]) res

    Annot { expr, typ } -> do
      setPatternMatching False
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = annotPrec
                                 , associativity = annotAssoc
                                 , direction     = Associativity.Left
                                 }

      let exprPretty = bracketPatternMatching (grouped $ pretty expr)

      let typPretty  = bracketPatternMatching (grouped $ align (pretty typ))

      maybeExprParen prevPrecAssoc (exprPretty <+> colon <+> typPretty)

    Andalso { lhs, rhs } -> do
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = andalsoPrec
                                   , associativity = andalsoAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- bracketPatternMatching (grouped $ pretty lhs)
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- bracketPatternMatching (grouped $ pretty rhs)
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      let res =
            [lhsPretty, "andalso" <+> rhsPretty]
              |> sequence
              |> vsep
              |> maybeExprParen prevPrecAssoc
      groupedIf prevPrecAssoc (`elem` [orelsePrec, andalsoPrec]) res

    Orelse { lhs, rhs } -> do
      setPatternMatching False
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = orelsePrec
                                   , associativity = orelseAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      lhsDoc <- bracketPatternMatching (grouped $ pretty lhs)
      let lhsPretty = return lhsDoc

      setExprPrecAssoc newPrecAssoc { direction = Associativity.Right }
      rhsDoc <- bracketPatternMatching (grouped $ pretty rhs)
      let rhsPretty = return rhsDoc

      setExprPrecAssoc newPrecAssoc

      let res =
            [lhsPretty, "orelse", rhsPretty]
              |> sequence
              |> vsep
              |> maybeExprParen prevPrecAssoc
      groupedIf prevPrecAssoc (`elem` [orelsePrec, andalsoPrec]) res

    Handle { expr, match } -> do
      prevPatternMatching <- getPatternMatching

      prevPrecAssoc       <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = handlePrec
                                 , associativity = handleAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprPatternMatchingParen
        prevPrecAssoc
        prevPatternMatching
        (grouped $ grouped (pretty expr) <> line <> "handle" <+> pretty match)

    Raise expr -> do
      setPatternMatching False
      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = raisePrec
                                 , associativity = raiseAssoc
                                 , direction     = Associativity.Right
                                 }

      let exprPretty = bracketPatternMatching (grouped $ pretty expr)

      maybeExprParen prevPrecAssoc (startsWith "raise" <+> exprPretty)

    If { cond, ifExpr, elseExpr } -> do
      prevPatternMatching <- getPatternMatching
      setPatternMatching False
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = ifPrec
                                   , associativity = ifAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      condDoc <- grouped (pretty cond)
      let condPretty = return condDoc

      setExprPrecAssoc newPrecAssoc
      ifExprDoc <- grouped (pretty ifExpr)
      let ifExprPretty = return ifExprDoc

      resetExprPrecAssoc
      setPatternMatching prevPatternMatching
      ungroupedElseExprDoc <- bracketPatternMatching (pretty elseExpr)
      let ungroupedElseExprPretty = return ungroupedElseExprDoc
      let elseExprPretty          = grouped ungroupedElseExprPretty

      setExprPrecAssoc newPrecAssoc
      [ startsWith "if "
        , align condPretty
        , " then"
        , nest (line <> ifExprPretty)
        , line
        , "else"
        -- Special handling for nested if/then/else
        , case Marked.value elseExpr of
          If{} -> space <> ungroupedElseExprPretty
          _    -> nest (line <> elseExprPretty)
        ]
        |> sequence
        |> hcat
        |> maybeExprParen prevPrecAssoc

    While { cond, body } -> do
      prevPatternMatching <- getPatternMatching
      setPatternMatching False
      prevPrecAssoc <- getExprPrecAssoc
      let newPrecAssoc = PrecAssoc { precedence    = whilePrec
                                   , associativity = whileAssoc
                                   , direction     = Associativity.Left
                                   }

      setExprPrecAssoc newPrecAssoc
      condDoc <- grouped (pretty cond)
      let condPretty = return condDoc

      resetExprPrecAssoc
      setPatternMatching prevPatternMatching
      bodyDoc <- bracketPatternMatching (grouped $ pretty body)
      let bodyPretty = return bodyDoc

      setExprPrecAssoc newPrecAssoc
      [startsWith "while ", align condPretty, " do", nest (line <> bodyPretty)]
        |> sequence
        |> hcat
        |> maybeExprParen prevPrecAssoc

    Case { expr, match } -> do
      prevPatternMatching <- getPatternMatching
      setPatternMatching False

      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = casePrec
                                 , associativity = caseAssoc
                                 , direction     = Associativity.Left
                                 }
      maybeExprPatternMatchingParen
        prevPrecAssoc
        prevPatternMatching
        (startsWith "case" <+> grouped (pretty expr) <+> "of" <> nest
          (hardline <> pretty match)
        )

    Fn { match } -> do
      prevPatternMatching <- getPatternMatching
      setPatternMatching False

      prevPrecAssoc <- getExprPrecAssoc
      setExprPrecAssoc PrecAssoc { precedence    = fnPrec
                                 , associativity = fnAssoc
                                 , direction     = Associativity.Right
                                 }
      maybeExprPatternMatchingParen prevPrecAssoc
                                    prevPatternMatching
                                    (startsWith "fn" <+> align (pretty match))

instance Pretty Row where
  pretty Row { label, expr } =
    pretty label <+> equals <+> (align . grouped $ pretty expr)

instance Pretty Match where
  pretty arms = do
    setPatternMatching True
    arms
      |> NonEmpty.toList
      |> mapM (withMatchClauses numClauses . pretty)
      |> punctuate' separator
      |> vhard
      |> hangBy (-2)
   where
    numClauses = length arms
    separator  = "| "

instance Pretty MatchArm where
  pretty MatchArm { lhs, rhs } = grouped (pretty lhs) <+> "=>" <> grouped
    (nest $ line <> grouped prettyRhs)
   where
    prettyRhs = do
      multipleMatchClauses <- getMultipleMatchClauses
      setPatternMatching multipleMatchClauses
      res <- pretty rhs
      setPatternMatching False
      return res

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
