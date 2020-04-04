module Parser.Internal.Parsers.Type
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

import           Ast.Typ                        ( MTyp )
import qualified Ast.Typ                       as Typ
import qualified Common.Marked                 as Marked
import           Parser.Internal.Basic   hiding ( Parser )
import           Parser.Internal.Parsers.Identifier
                                                ( typeVariable
                                                , typeConstructor
                                                , label
                                                , long
                                                )
import qualified Parser.Internal.Token         as Token

typ :: (MonadParser parser) => parser MTyp
typ = dbg ["typ"] $ E.makeExprParser typ' operatorTable
 where
  -- Handle left-recursive tycon application
  typ' = do
    arg         <- typ''
    -- @try@ to prevent trying to parse * as tycon
    maybeTycons <- many (long $ try typeConstructor)
    return $ case nonEmpty maybeTycons of
      Nothing     -> arg
      Just tycons -> Marked.merge
        arg
        (last tycons)
        (Typ.App { Typ.args = arg :| [], Typ.tycons })

  -- Non-left recursive cases
  typ''         = choice [multiArgApp, parens, unappliedTycon, tyvar, record]

  operatorTable = reverse
    [
      -- Arrow type
      -- Flatten chains when possible
      [ let arrow t1 t2 = Marked.merge
              t1
              t2
              (case Marked.value t2 of
                Typ.Arrow ts -> Typ.Arrow (t1 `NonEmpty.cons` ts)
                _ -> Typ.Arrow (t1 :| [t2])
              )
            separator = token_ Token.Narrowarrow
        in  E.InfixR (arrow <$ separator)
      ]
      -- Product type
      -- Flatten chains when possible
    , [ let tup t1 t2 = Marked.merge
              t1
              t2
              (case Marked.value t2 of
                Typ.Tuple ts -> Typ.Tuple (t1 `NonEmpty.cons` ts)
                _ -> Typ.Tuple (t1 :| [t2])
              )
            separator = tokenWith $ \case
              Token.Symbolic "*" -> Just ()
              _ -> Nothing
        in  E.InfixR (tup <$ separator)
      ]
    ]

-- Application where left recursion isn't an issue
multiArgApp :: (MonadParser parser) => parser MTyp
multiArgApp = dbg ["typ", "multiArgApp"] . marked . try $ do
  args   <- parenthesized (typ `sepBy1` token_ Token.Comma)
  -- @try@ to prevent trying to parse * as tycon
  tycons <- some (long $ try typeConstructor)

  return Typ.App { Typ.tycons, Typ.args }

-- Parenthesized
parens :: (MonadParser parser) => parser MTyp
parens = parenthesized typ

unappliedTycon :: (MonadParser parser) => parser MTyp
-- @try@ to prevent conflict with *
unappliedTycon = marked $ Typ.TyCon <$> try (long typeConstructor)

tyvar :: (MonadParser parser) => parser MTyp
tyvar = dbg ["typ", "tyvar"] . marked $ Typ.TyVar <$> typeVariable

record :: (MonadParser parser) => parser MTyp
record = dbg ["typ", "record"] . marked $ Typ.Record <$> braces
  (row `sepBy` token_ Token.Comma)
 where
  row :: (MonadParser parser) => parser Typ.MRow
  row = marked $ do
    lbl <- label
    token_ Token.Colon
    t <- typ
    return Typ.Row { Typ.label = lbl, Typ.typ = t }
