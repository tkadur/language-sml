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

import           Ast.Typ                        ( Typ )
import qualified Ast.Typ                       as Typ
import           Parser.Internal.Basic
import           Parser.Internal.Parsers.Identifier
                                                ( typeVariable
                                                , typeConstructor
                                                , label
                                                , long
                                                )
import qualified Parser.Internal.Token         as Token

typ :: Parser Typ
typ = dbg ["typ"] $ E.makeExprParser typ' operatorTable
 where
  -- Handle left-recursive tycon application
  typ' = do
    arg         <- typ''
    -- @try@ to prevent trying to parse * as tycon
    maybeTycons <- many (long $ try typeConstructor)
    return $ case maybeTycons of
      [] -> arg
      tycon : tycons ->
        Typ.App { Typ.args = arg :| [], Typ.tycons = tycon :| tycons }

  -- Non-left recursive cases
  typ''         = choice [multiArgApp, parens, unappliedTycon, tyvar, record]

  operatorTable = reverse
    [
      -- Arrow type
      -- Flatten chains when possible
      [ let arrow t1 t2 = case t2 of
              Typ.Arrow ts -> Typ.Arrow (t1 `NonEmpty.cons` ts)
              _ -> Typ.Arrow (t1 :| [t2])
            separator = token_ Token.Narrowarrow
        in  E.InfixR (arrow <$ separator)
      ]
      -- Product type
      -- Flatten chains when possible
    , [ let tup t1 t2 = case t2 of
              Typ.Tuple ts -> Typ.Tuple (t1 `NonEmpty.cons` ts)
              _ -> Typ.Tuple (t1 :| [t2])
            separator = tokenWith $ \case
              Token.Symbolic "*" -> Just ()
              _ -> Nothing
        in  E.InfixR (tup <$ separator)
      ]
    ]

-- Application where left recursion isn't an issue
multiArgApp :: Parser Typ
multiArgApp = dbg ["typ", "multiArgApp"] . try $ do
  args   <- parenthesized (typ `sepBy1` token_ Token.Comma)
  -- @try@ to prevent trying to parse * as tycon
  tycons <- some (long $ try typeConstructor)

  return Typ.App { Typ.tycons, Typ.args }

-- Parenthesized
parens :: Parser Typ
parens = parenthesized typ

unappliedTycon :: Parser Typ
-- @try@ to prevent conflict with *
unappliedTycon = Typ.TyCon <$> try (long typeConstructor)

tyvar :: Parser Typ
tyvar = dbg ["typ", "tyvar"] $ Typ.TyVar <$> typeVariable

record :: Parser Typ
record = dbg ["typ", "record"] $ Typ.Record <$> braces
  (row `sepBy` token_ Token.Comma)
 where
  row :: Parser Typ.Row
  row = do
    lbl <- label
    token_ Token.Colon
    t <- typ
    return Typ.Row { Typ.label = lbl, Typ.typ = t }
