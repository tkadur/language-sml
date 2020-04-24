module Language.Sml.Parser.Internal.Parsers.Type
  ( typ
  )
where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                , sepBy
                                                )
import           Control.Applicative.Combinators.NonEmpty
                                                ( some
                                                , sepBy1
                                                )
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.List.NonEmpty            as NonEmpty
import           Text.Megaparsec                ( try )

import           Language.Sml.Ast.Typ           ( MTyp )
import qualified Language.Sml.Ast.Typ          as Typ
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.Parsers.Identifier
                                                ( typeVariable
                                                , typeConstructor
                                                , label
                                                , long
                                                )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

typ :: Parser MTyp
typ = dbg ["typ"] $ E.makeExprParser typ' operatorTable
 where
  -- Handle product types
  typ' = do
    typs <- typ'' `sepBy1` token_ (Token.Symbolic "*")
    return $ case typs of
      t :| [] -> t
      _ ->
        Marked.merge (NonEmpty.head typs) (NonEmpty.last typs) (Typ.Tuple typs)

  -- Handle left-recursive tycon application
  typ'' = do
    arg         <- typ'''
    -- @try@ to prevent trying to parse * as tycon
    maybeTycons <- many (long $ try typeConstructor)
    return $ case nonEmpty maybeTycons of
      Nothing     -> arg
      Just tycons -> Marked.merge
        arg
        (last tycons)
        (Typ.App { Typ.args = arg :| [], Typ.tycons })

  -- Non-left recursive cases
  typ'''        = choice [multiArgApp, parens, unappliedTycon, tyvar, record]

  operatorTable = reverse
    [
      -- Arrow type
      [ let arrow lhs rhs =
              Marked.merge lhs rhs (Typ.Arrow { Typ.lhs, Typ.rhs })
            separator = token_ Token.Narrowarrow
        in  E.InfixR (arrow <$ separator)
      ]
    ]

-- Application where left recursion isn't an issue
multiArgApp :: Parser MTyp
multiArgApp = dbg ["typ", "multiArgApp"] . marked . try $ do
  args   <- parenthesized (typ `sepBy1` token_ Token.Comma)
  -- @try@ to prevent trying to parse * as tycon
  tycons <- some (long $ try typeConstructor)

  return Typ.App { Typ.tycons, Typ.args }

-- Parenthesized
parens :: Parser MTyp
parens = parenthesized typ

unappliedTycon :: Parser MTyp
-- @try@ to prevent conflict with *
unappliedTycon = marked $ Typ.TyCon <$> try (long typeConstructor)

tyvar :: Parser MTyp
tyvar = dbg ["typ", "tyvar"] . marked $ Typ.TyVar <$> typeVariable

record :: Parser MTyp
record = dbg ["typ", "record"] . marked $ Typ.Record <$> braces
  (row `sepBy` token_ Token.Comma)
 where
  row :: Parser Typ.MRow
  row = marked $ do
    lbl <- label
    token_ Token.Colon
    t <- typ
    return Typ.Row { Typ.label = lbl, Typ.typ = t }
