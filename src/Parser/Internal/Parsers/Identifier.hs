module Parser.Internal.Parsers.Identifier
  ( valueIdentifier
  , nonfixValueIdentifier
  , structureIdentifier
  , typeVariable
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
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import qualified Parser.Internal.Token         as Token

-- | Parses a value identifier which must not be infixed
nonfixValueIdentifier :: FixityTable -> Parser (Op (Long ValueIdent))
nonfixValueIdentifier fixityTable = do
  x <- op . long $ valueIdentifier
  case x of
    Op.Ident Long.Long { Long.qualifiers = [], Long.ident } ->
      if ident `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show ident
        else return x
    _ -> return x

-- | Parses a value identifier
valueIdentifier :: Parser ValueIdent
valueIdentifier = ValueIdent.ValueIdent <$> identifier

-- | Parses a structure identifier
structureIdentifier :: Parser StructureIdent
structureIdentifier = StructureIdent.StructureIdent <$> alphanumeric

typeVariable :: Parser TyVar
typeVariable = do
  ticks <- some (token_ Token.Tick)
  let leadingPrimes = Positive.positive (NonEmpty.length ticks)

  ident <- alphanumeric

  return TyVar.TyVar { TyVar.ident, TyVar.leadingPrimes }

-- | Parses an identifier possible prefixed by op
op :: Parser ident -> Parser (Op ident)
op ident = choice [with, without]
 where
  with = do
    token_ Token.Op
    Op.Op <$> ident

  without = Op.Ident <$> ident

-- | Parses a possibly qualified identifier
long :: forall ident . (Show ident) => Parser ident -> Parser (Long ident)
long p = dbg ["longIdentifier"] $ do
  qualifiers <- quals
  ident      <- p
  return Long.Long { Long.qualifiers, Long.ident = ident }
 where
  qual :: Parser StructureIdent
  qual = do
    qualifier <- structureIdentifier
    token_ Token.Dot
    return qualifier

  quals :: Parser [StructureIdent]
  -- @try@ so we don't fail once we reach the end of the qualifiers
  quals = many (try qual)

identifier :: Parser Text
identifier = alphanumeric <|> symbolic

-- | Alphanumeric identifiers
alphanumeric :: Parser Text
alphanumeric = dbg ["alphanumeric"] $ tokenWith
  (\case
    Token.Alphanumeric s -> Just s
    _ -> Nothing
  )

-- | Symbolic identifiers
symbolic :: Parser Text
symbolic = dbg ["symbolic"] $ tokenWith
  (\case
    Token.Symbolic s -> Just s
    _ -> Nothing
  )
