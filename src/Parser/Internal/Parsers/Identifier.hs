module Parser.Internal.Parsers.Identifier
  ( valueIdentifier
  , nonfixValueIdentifier
  , nonfixLongValueIdentifier
  , structureIdentifier
  , typeVariable
  , typeConstructor
  , label
  , long
  , op
  )
where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( some )
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import           Text.Megaparsec                ( try )

import           Ast.Ident.Label                ( Label )
import qualified Ast.Ident.Label               as Label
import           Ast.Ident.Long                 ( Long )
import qualified Ast.Ident.Long                as Long
import           Ast.Ident.Op                   ( Op )
import qualified Ast.Ident.Op                  as Op
import           Ast.Ident.TyCon                ( TyCon )
import qualified Ast.Ident.TyCon               as TyCon
import           Ast.Ident.TyVar                ( TyVar )
import qualified Ast.Ident.TyVar               as TyVar
import           Ast.Ident.StructureIdent       ( StructureIdent )
import qualified Ast.Ident.StructureIdent      as StructureIdent
import           Ast.Ident.ValueIdent           ( ValueIdent )
import qualified Ast.Ident.ValueIdent          as ValueIdent
import qualified Common.Positive               as Positive
import           Parser.Internal.Basic   hiding ( Parser )
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Literal
                                                ( decimal )
import qualified Parser.Internal.Token         as Token

-- | Parses a value identifier which must not be infixed
nonfixValueIdentifier :: (MonadParser parser)
                      => FixityTable
                      -> parser (Op ValueIdent)
nonfixValueIdentifier fixityTable = do
  x <- op valueIdentifier
  case x of
    Op.Ident ident ->
      if ident `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show ident
        else return x
    _ -> return x

-- | Parses a long value identifier which must not be infixed
nonfixLongValueIdentifier :: (MonadParser parser)
                          => FixityTable
                          -> parser (Op (Long ValueIdent))
nonfixLongValueIdentifier fixityTable = do
  x <- op (long valueIdentifier)
  case x of
    Op.Ident Long.Long { Long.qualifiers = [], Long.ident } ->
      if ident `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show ident
        else return x
    _ -> return x

-- | Parses a value identifier
valueIdentifier :: (MonadParser parser) => parser ValueIdent
valueIdentifier = ValueIdent.ValueIdent <$> identifier

-- | Parses a structure identifier
structureIdentifier :: (MonadParser parser) => parser StructureIdent
structureIdentifier = StructureIdent.StructureIdent <$> alphanumeric

typeVariable :: (MonadParser parser) => parser TyVar
typeVariable = do
  ticks <- some (token_ Token.Tick)
  let leadingPrimes = Positive.positive (NonEmpty.length ticks)

  ident <- alphanumeric

  return TyVar.TyVar { TyVar.ident, TyVar.leadingPrimes }

typeConstructor :: (MonadParser parser) => parser TyCon
typeConstructor = do
  ident <- identifier
  case ident of
    "*" -> fail "* is not a valid type constructor"
    _   -> return (TyCon.TyCon ident)

label :: (MonadParser parser) => parser Label
label = ident <|> numeric
 where
  ident   = Label.Ident <$> identifier

  numeric = do
    number <- decimal
    if number <= 0
      then fail "record labels cannot be nonpositive"
      else return $ Label.Numeric (Positive.positive number)

-- | Parses an identifier possible prefixed by op
op :: (MonadParser parser) => parser ident -> parser (Op ident)
op ident = choice [with, without]
 where
  with = do
    token_ Token.Op
    Op.Op <$> ident

  without = Op.Ident <$> ident

-- | Parses a possibly qualified identifier
long :: (MonadParser parser, Show ident) => parser ident -> parser (Long ident)
long p = dbg ["longIdentifier"] $ do
  qualifiers <- quals
  ident      <- p
  return Long.Long { Long.qualifiers, Long.ident = ident }
 where
  qual :: (MonadParser parser) => parser StructureIdent
  qual = do
    qualifier <- structureIdentifier
    token_ Token.Dot
    return qualifier

  quals :: (MonadParser parser) => parser [StructureIdent]
  -- @try@ so we don't fail once we reach the end of the qualifiers
  quals = many (try qual)

identifier :: (MonadParser parser) => parser Text
identifier = alphanumeric <|> symbolic

-- | Alphanumeric identifiers
alphanumeric :: (MonadParser parser) => parser Text
alphanumeric = dbg ["alphanumeric"] $ tokenWith
  (\case
    Token.Alphanumeric s -> Just s
    _ -> Nothing
  )

-- | Symbolic identifiers
symbolic :: (MonadParser parser) => parser Text
symbolic = dbg ["symbolic"] $ tokenWith
  (\case
    Token.Symbolic s -> Just s
    _ -> Nothing
  )
