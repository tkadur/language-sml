module Parser.Internal.Parsers.Identifier where

import           Control.Monad.Combinators      ( choice )
import           Control.Monad.Combinators.NonEmpty
                                                ( endBy1 )
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Relude.Unsafe                 as Unsafe
import           Text.Megaparsec                ( try )

import           Ast.Ident.Ident                ( Ident )
import qualified Ast.Ident.Ident               as Ident
import           Ast.Ident.LongIdent            ( LongIdent )
import qualified Ast.Ident.LongIdent           as LongIdent
import           Ast.Ident.ValueIdent           ( ValueIdent )
import qualified Ast.Ident.ValueIdent          as ValueIdent
import           Parser.Internal.Basic
import           Parser.Internal.Combinators    ( sepBy2 )
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import qualified Parser.Internal.Token         as Token

-- | Parses a value identifier which must not be infixed
nonfixValueIdentifier :: FixityTable -> Parser ValueIdent
nonfixValueIdentifier fixityTable = do
  ident <- valueIdentifier
  case ident of
    ValueIdent.LongIdent (LongIdent.Ident x) ->
      if x `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show x
        else return ident
    _ -> return ident

-- | Parses a possibly qualified, possible "op"-prefixed identifier
valueIdentifier :: Parser ValueIdent
valueIdentifier = dbg ["valueIdentifier"] $ choice [op, longIdent]
 where
  op = do
    -- M.try in case a longIdent starts with "op"
    token_ Token.Op
    ValueIdent.Op <$> valueIdentifier

  longIdent = ValueIdent.LongIdent <$> longIdentifier

-- | Parses a possibly qualified identifier
longIdentifier :: Parser LongIdent
longIdentifier = dbg ["longIdentifier"]
  $ choice [qualifiedAlphanum, qualifiedSymbolic, unqualified]
 where
  -- | Qualified identifier ending in an alphanumeric identifier
  qualifiedAlphanum :: Parser LongIdent
  -- @try@ because this could consume a valid bare identifier
  qualifiedAlphanum = dbg ["longIdentifier", "qualifiedNonfix"] . try $ do
    idents <- alphanumeric `sepBy2` token_ Token.Dot
    let ident = NonEmpty.last idents
    let qualifiers =
          idents
            |> NonEmpty.take (NonEmpty.length idents - 1)
            |> NonEmpty.nonEmpty
            |> Unsafe.fromJust
    return LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident }

  -- | Qualified identifier ending in a symbolic identifier
  qualifiedSymbolic :: Parser LongIdent
    -- @try@ because this could consume a valid bare identifier
  qualifiedSymbolic = dbg ["longIdentifier", "qualifiedInfix"] . try $ do
    -- @try@ to prevent this from trying and failing to consume the symbolic identifier
    qualifiers <- endBy1 (try alphanumeric) (token_ Token.Dot)
    ident      <- bareIdentifier
    return LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident }

  unqualified :: Parser LongIdent
  unqualified = LongIdent.Ident <$> bareIdentifier

-- | Parses a bare identifier
bareIdentifier :: Parser Ident
bareIdentifier = alphanumeric <|> symbolic

-- | Alphanumeric identifiers
alphanumeric :: Parser Ident
alphanumeric = dbg ["alphanumeric"] $ tokenWith
  (\case
    Token.Alphanumeric s -> Just $ Ident.Ident s
    _ -> Nothing
  )

-- | Symbolic identifiers
symbolic :: Parser Ident
symbolic = dbg ["symbolic"] $ tokenWith
  (\case
    Token.Symbolic s -> Just $ Ident.Ident s
    _ -> Nothing
  )
