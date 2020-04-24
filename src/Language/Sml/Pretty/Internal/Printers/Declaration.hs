{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sml.Pretty.Internal.Printers.Declaration where

import qualified Data.List.NonEmpty            as NonEmpty

import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Decl
import           Language.Sml.Ast.Ident.TyVar   ( )
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

-- See Expression.hs for explanation of grouping logic.

instance Pretty Decl where
  -- It's safe to blanket group @Decl@s since they don't need to consolidate
  -- chunks of the AST like @Expr@s/@Pat@s/@Typ@s
  pretty = grouped . \case
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

    TypAlias { typbinds } -> startsWith "type" <+> binds typbinds

    Datatype { datbinds, withtype } ->
      let withtypePretty = case withtype of
            Nothing       -> emptyDoc
            Just typbinds -> hardline <> withtypeBinds typbinds
      in  startsWith "datatype" <+> binds datbinds <> withtypePretty

    DatatypeReplication { new, old } ->
      startsWith "datatype" <+> pretty new <+> equals <> grouped
        (nest $ line <> "datatype" <+> pretty old)

    Abstype { datbinds, withtype, decl } ->
      let withtypePretty = case withtype of
            Nothing       -> emptyDoc
            Just typbinds -> hardline <> withtypeBinds typbinds
      in  startsWith "abstype"
            <+> binds datbinds
            <>  withtypePretty
            <>  hardline
            <>  "with"
            <>  nest (hardline <> pretty decl)
            <>  hardline
            <>  "end"

    Exception { exnbinds } -> startsWith "exception" <+> binds exnbinds

    Local { decl, body } ->
      [ startsWith "local"
        , nest (line <> pretty decl)
        , line
        , "in"
        , nest (line <> pretty body)
        , line
        , endsWith "end"
        ]
        |> sequence
        |> hcat

    Open strids ->
      let structureIdents = NonEmpty.toList strids
      in  startsWith "open" <+> alignsep (mapM pretty structureIdents)

    Sequence decls -> prettyPreservingNewlines decls

    Infix { precedence, idents } ->
      let precedencePretty = case precedence of
            Nothing   -> emptyDoc
            Just prec -> space <> pretty prec
          identifiers = NonEmpty.toList idents
      in  startsWith "infix" <> precedencePretty <+> alignsep
            (mapM pretty identifiers)

    Infixr { precedence, idents } ->
      let precedencePretty = case precedence of
            Nothing   -> emptyDoc
            Just prec -> space <> pretty prec
          identifiers = NonEmpty.toList idents
      in  startsWith "infixr" <> precedencePretty <+> alignsep
            (mapM pretty identifiers)

    Nonfix { idents } ->
      let identifiers = NonEmpty.toList idents
      in  startsWith "nonfix" <+> alignsep (mapM pretty identifiers)

instance Pretty ValBind where
  pretty ValBind { isRec, lhs, rhs } =
    recPretty <> grouped (nest $ pretty lhs) <+> equals <> grouped
      (nest $ line <> pretty rhs)
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
      let infixPart' = prettyArg lhs <+> pretty infixName <+> prettyArg rhs
          -- We can omit parens around the infix part if there are no other arguments
          infixPart  = case infixArgs of
            [] -> infixPart'
            _  -> parens infixPart'

          -- There may not be other args, so we need to handle spacing
          argsPretty = case infixArgs of
            [] -> emptyDoc
            _  -> space <> alignsep (mapM prettyArg infixArgs)

          -- There may not be a return type, so we need to handle spacing
          returnTypPretty = case returnTyp of
            Nothing  -> emptyDoc
            Just typ -> line <> colon <+> grouped (align $ pretty typ)

          bodyPretty = do
            setPatternMatching True
            nest (line <> grouped (pretty body))
      -- Only allow the body to be inline with the "=" if the whole thing fits on one line
      in  grouped
            -- Only allow the return type to be inline with the args if they all fit on one line
            (infixPart <> argsPretty <> returnTypPretty <+> equals <> bodyPretty
            )
    NonfixClause { nonfixName, nonfixArgs, returnTyp, body } ->
      let args       = NonEmpty.toList nonfixArgs
          argsPretty = alignsep (mapM prettyArg args)

            -- There may not be a return type, so we need to handle spacing
          returnTypPretty = case returnTyp of
            Nothing  -> emptyDoc
            Just typ -> line <> colon <+> grouped (align $ pretty typ)

          bodyPretty = do
            setPatternMatching True
            nest (line <> grouped (pretty body))
      -- Only allow the body to be inline with the "=" if the whole thing fits on one line
      in  grouped
            -- Only allow the return type to be inline with the args if they all fit on one line
            (   pretty nonfixName
            <+> argsPretty
            <>  returnTypPretty
            <+> equals
            <>  bodyPretty
            )
   where
    -- Hack to make argument parenthesized when necessary
    -- Pretend that they're inside a higher-than-maximum precedence
    -- operator
    prettyArg arg = do
      setExprPrecAssoc PrecAssoc { precedence    = 11
                                 , associativity = Associativity.Left
                                 , direction     = Associativity.Right
                                 }
      res <- grouped (pretty arg)
      resetExprPrecAssoc
      return res

instance Pretty TypBind where
  pretty TypBind { tyvars, tycon, typ } =
    tyvarsPretty <> pretty tycon <+> equals <> grouped
      (nest $ line <> pretty typ)
   where
     -- Deal with parenthesization and spacing of 0, 1, or many tyvars
    tyvarsPretty = case tyvars of
      []      -> emptyDoc
      [tyvar] -> pretty tyvar <> space
      _       -> tupled (mapM pretty tyvars) <> space

instance Pretty DatBind where
  pretty DatBind { tyvars, tycon, conbinds } =
    tyvarsPretty <> pretty tycon <> conbindsPretty
   where
    conbindsPretty =
      conbinds
        |> NonEmpty.toList
        |> mapM pretty
        |> punctuate' "| "
        |> vsep
        -- Slight hack to get = to line up with |
        |> ((line <> equals) <+>)
        |> nest
        |> grouped

     -- Deal with parenthesization and spacing of 0, 1, or many tyvars
    tyvarsPretty = case tyvars of
      []      -> emptyDoc
      [tyvar] -> pretty tyvar <> space
      _       -> tupled (mapM pretty tyvars) <> space

instance Pretty ConBind where
  pretty ConBind { constructor, arg } =
    let prettyArg = case arg of
          Nothing -> emptyDoc
          Just typ ->
            space <> "of" <> grouped (nest $ line <> grouped (pretty typ))
    in  pretty constructor <> prettyArg

instance Pretty ExnBind where
  -- Unlike with conbinds, in case an exnbind doesn't fit on a line
  -- we want to nest the entire exnbind on the next line
  pretty = grouped . nest . (line' <>) . \case
    ExnBind { constructor, arg } ->
      let prettyArg = case arg of
            Nothing  -> emptyDoc
            Just typ -> space <> "of" <+> grouped (pretty typ)
      in  pretty constructor <> prettyArg
    ExnReplication { new, old } -> pretty new <+> equals <+> pretty old

binds :: (Pretty a, Show a) => NonEmpty (Marked a) -> Doc ann
binds = \case
  firstBind :| [] -> pretty firstBind
  firstBind :| andBinds ->
    let andBindsPretty = map
          (\andBind@Marked.Marked { Marked.startPosition } ->
            -- Anchor "and" to the following bind instead of the previous one
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

withtypeBinds :: MTypBinds -> Doc ann
withtypeBinds typbinds = prefix <+> binds typbinds
 where
  -- Anchor "withtype" to the following bind instead of the previous one
  prefix = pretty $ Marked.Marked { Marked.value         = "withtype" :: Text
                                  , Marked.startPosition
                                  , Marked.endPosition   = startPosition
                                  }
  Marked.Marked { Marked.startPosition } :| _ = typbinds
