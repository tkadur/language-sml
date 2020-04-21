{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Declaration where

import qualified Data.List.NonEmpty            as NonEmpty

import qualified Language.Sml.Ast.Associativity
                                               as Associativity
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
    Fun { tyvars, funbinds } ->
      let preamble = case tyvars of
            []      -> startsWith "fun"
            [tyvar] -> startsWith "fun" <+> pretty tyvar
            _       -> startsWith "fun" <+> tupled (mapM pretty tyvars)
      in  preamble <+> binds funbinds

instance Pretty ValBind where
  pretty ValBind { isRec, lhs, rhs } =
    recPretty <> nest (pretty lhs) <+> equals <> nest (line <> pretty rhs)
    where recPretty = if isRec then startsWith "rec " else emptyDoc

instance Pretty FunBind where
  pretty FunBind { clauses } =
    clauses
      |> NonEmpty.toList
      |> mapM pretty
      |> punctuate' separator
      |> vhard
      |> hangBy (-2)
    where separator = "| "

instance Pretty FunClause where
  pretty = \case
    InfixClause { lhs, infixName, rhs, infixArgs, returnTyp, body } ->
      let
        infixPart' = prettyArg lhs <+> pretty infixName <+> prettyArg rhs
        -- We can omit parens around the infix part if there are no other arguments
        infixPart  = case infixArgs of
          [] -> infixPart'
          _  -> parens infixPart'

        -- There may not be other args, so we need to handle spacing
        argsPretty = case infixArgs of
          [] -> emptyDoc
          _  -> space <> align (grouped . vsep $ mapM prettyArg infixArgs)

        -- There may not be a return type, so we need to handle spacing
        returnTypPretty = maybe
          emptyDoc
          (\typ -> space <> colon <+> align (pretty typ))
          returnTyp

        bodyPretty = grouped (nest . nest $ line <> pretty body)
      in
        infixPart <> argsPretty <> returnTypPretty <+> equals <> bodyPretty
    NonfixClause { nonfixName, nonfixArgs, returnTyp, body } ->
      let
        args       = NonEmpty.toList nonfixArgs
        argsPretty = align (grouped . vsep $ mapM prettyArg args)

          -- There may not be a return type, so we need to handle spacing
        returnTypPretty = maybe
          emptyDoc
          (\typ -> space <> colon <+> align (pretty typ))
          returnTyp

        bodyPretty = grouped (nest . nest $ line <> pretty body)
      in
        pretty nonfixName
        <+> argsPretty
        <>  returnTypPretty
        <+> equals
        <>  bodyPretty
   where
    -- Hack to make argument parenthesized when necessary
    -- Pretend that they're inside a higher-than-maximum precedence
    -- operator
    prettyArg arg = do
      setExprPrecAssoc PrecAssoc { precedence    = 11
                                 , associativity = Associativity.Left
                                 , direction     = Associativity.Right
                                 }
      res <- pretty arg
      resetExprPrecAssoc
      return res


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
