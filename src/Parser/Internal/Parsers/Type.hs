module Parser.Internal.Parsers.Type
  ( typ
  )
where

import           Control.Monad.Combinators      ( choice
                                                , sepBy
                                                )
import           Control.Applicative.Combinators.NonEmpty
                                                ( some
                                                , sepBy1
                                                )
import qualified Control.Monad.Combinators.Expr
                                               as E
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
  typ'          = choice [multiArgApp, parens, unappliedTycon, tyvar, record]

  operatorTable = reverse
    [
      -- Arrow type
      [E.InfixR (Typ.Arrow <$ token_ Token.Narrowarrow)]
      -- Product type
    , [ let tup t1 t2 = Typ.Tuple (t1 :| [t2])
            separator = tokenWith $ \case
              Token.Symbolic "*" -> Just ()
              _ -> Nothing
        in  E.InfixL (tup <$ separator)
      ]
    , -- Type constructor application
      -- TODO(tkadur) parse this properly instead of letting RHS be an arbitrary type
      [ let app arg tycon = Typ.App { Typ.args = arg :| [], Typ.tycon }
            separator = nothing
        in  E.InfixL (app <$ separator)
      ]
    ]

-- Application where left recursion isn't an issue
multiArgApp :: Parser Typ
multiArgApp = dbg ["typ", "multiArgApp"] . try $ do
  args  <- parenthesized (typ `sepBy1` token_ Token.Comma)
  tycon <- Typ.TyCon <$> long typeConstructor

  return Typ.App { Typ.tycon, Typ.args }

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
