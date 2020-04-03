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
import qualified Data.List.NonEmpty            as NonEmpty
import           Text.Megaparsec                ( try )

import           Ast.Typ                        ( Typ )
import qualified Ast.Typ                       as Typ
import           Parser.Internal.Basic
import           Parser.Internal.Combinators    ( sepBy2 )
import           Parser.Internal.Parsers.Identifier
                                                ( typeVariable
                                                , typeConstructor
                                                , label
                                                , long
                                                )
import qualified Parser.Internal.Token         as Token

typ :: Parser Typ
typ = dbg ["typ"] maybeSingleArgApp
 where
  -- Handle case of left recursive application
  maybeSingleArgApp = do
    arg         <- typ'
    maybeTycons <- optional $ some (long typeConstructor)
    return $ case maybeTycons of
      Nothing -> arg
      Just (tycon :| tycons) -> foldl'
        (\t con -> Typ.App { Typ.tycon = con, args = t :| [] })
        (Typ.App { Typ.tycon, Typ.args = arg :| [] })
        tycons

  typ'          = E.makeExprParser typ'' operatorTable
  typ''         = choice [multiArgApp, parens, unappliedTycon, tyvar, record]

  operatorTable = reverse
    [
      -- Arrow type
      [E.InfixR (Typ.Arrow <$ token_ Token.Narrowarrow)]
      -- Product type
    , [ let tup t1 t2 = Typ.Tuple (t1 :| [t2])
            separator = tokenWith $ \case
              Token.Symbolic "*" -> Just ()
              _ -> Nothing
        in  (E.InfixL (tup <$ separator))
      ]
    ]

-- Application where left recursion isn't an issue
multiArgApp :: Parser Typ
multiArgApp = dbg ["typ", "multiArgApp"] . try $ do
  args  <- parenthesized (typ `sepBy1` token_ Token.Comma)
  tycon <- long typeConstructor

  return Typ.App { Typ.tycon, Typ.args }

-- Parenthesized
parens :: Parser Typ
parens = parenthesized typ

unappliedTycon :: Parser Typ
unappliedTycon = Typ.TyCon <$> long typeConstructor

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
