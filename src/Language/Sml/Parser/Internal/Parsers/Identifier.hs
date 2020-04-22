module Language.Sml.Parser.Internal.Parsers.Identifier
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

import           Language.Sml.Ast.Ident.Label   ( MLabel )
import qualified Language.Sml.Ast.Ident.Label  as Label
import           Language.Sml.Ast.Ident.Long    ( MLong )
import qualified Language.Sml.Ast.Ident.Long   as Long
import           Language.Sml.Ast.Ident.Op      ( MOp )
import qualified Language.Sml.Ast.Ident.Op     as Op
import           Language.Sml.Ast.Ident.TyCon   ( MTyCon )
import qualified Language.Sml.Ast.Ident.TyCon  as TyCon
import           Language.Sml.Ast.Ident.TyVar   ( MTyVar )
import qualified Language.Sml.Ast.Ident.TyVar  as TyVar
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( MStructureIdent )
import qualified Language.Sml.Ast.Ident.StructureIdent
                                               as StructureIdent
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( MValueIdent )
import qualified Language.Sml.Ast.Ident.ValueIdent
                                               as ValueIdent
import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Common.Marked    as Marked
import qualified Language.Sml.Common.Positive  as Positive
import           Language.Sml.Parser.Internal.Basic
                                         hiding ( Parser )
import           Language.Sml.Parser.Internal.FixityTable
                                                ( FixityTable )
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import           Language.Sml.Parser.Internal.Parsers.Literal
                                                ( decimal )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

-- | Parses a value identifier which must not be infixed
nonfixValueIdentifier :: (MonadParser parser)
                      => FixityTable
                      -> parser (MOp MValueIdent)
nonfixValueIdentifier fixityTable = do
  x <- op valueIdentifier
  case Marked.value x of
    Op.Ident ident ->
      if Marked.value ident `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show ident
        else return x
    _ -> return x

-- | Parses a long value identifier which must not be infixed
nonfixLongValueIdentifier :: (MonadParser parser)
                          => FixityTable
                          -> parser (MOp (MLong MValueIdent))
nonfixLongValueIdentifier fixityTable = do
  x <- op (long valueIdentifier)
  case Marked.value <$> Marked.value x of
    Op.Ident Long.Long { Long.qualifiers = [], Long.ident } ->
      if Marked.value ident `HashSet.member` FixityTable.operators fixityTable
        then fail $ "unexpected infix identifier " ++ show ident
        else return x
    _ -> return x

-- | Parses a value identifier
valueIdentifier :: (MonadParser parser) => parser MValueIdent
valueIdentifier = ValueIdent.ValueIdent <<$>> identifierOrEqual

-- | Parses a structure identifier
structureIdentifier :: (MonadParser parser) => parser MStructureIdent
structureIdentifier = StructureIdent.StructureIdent <<$>> alphanumeric

typeVariable :: (MonadParser parser) => parser MTyVar
typeVariable = marked $ do
  ticks <- some (token_ Token.Tick)
  let leadingPrimes = Positive.positive (NonEmpty.length ticks)

  ident <- Marked.value <$> alphanumeric

  return TyVar.TyVar { TyVar.ident, TyVar.leadingPrimes }

typeConstructor :: (MonadParser parser) => parser MTyCon
typeConstructor = do
  ident <- identifier
  case Marked.value ident of
    "*" -> fail "* is not a valid type constructor"
    _   -> return (TyCon.TyCon <$> ident)

label :: (MonadParser parser) => parser MLabel
label = ident <|> numeric
 where
  ident   = Label.Ident <<$>> identifier

  numeric = marked $ do
    number <- decimal
    if number <= 0
      then fail "record labels cannot be nonpositive"
      else return $ Label.Numeric (Positive.positive number)

-- | Parses an identifier possible prefixed by op
op :: (MonadParser parser) => parser ident -> parser (MOp ident)
op ident = marked $ choice [with, without]
 where
  with = do
    token_ Token.Op
    Op.Op <$> ident

  without = Op.Ident <$> ident

-- | Parses a possibly qualified identifier
long :: (MonadParser parser, Show ident) => parser ident -> parser (MLong ident)
long p = dbg ["longIdentifier"] . marked $ do
  qualifiers <- quals
  ident      <- p
  return Long.Long { Long.qualifiers, Long.ident = ident }
 where
  qual :: (MonadParser parser) => parser MStructureIdent
  qual = do
    qualifier <- structureIdentifier
    token_ Token.Dot
    return qualifier

  quals :: (MonadParser parser) => parser [MStructureIdent]
  -- @try@ so we don't fail once we reach the end of the qualifiers
  quals = many (try qual)

identifier :: (MonadParser parser) => parser (Marked Text)
identifier = alphanumeric <|> symbolic

identifierOrEqual :: (MonadParser parser) => parser (Marked Text)
identifierOrEqual = alphanumeric <|> symbolic <|> marked
  (tokenWith $ \case
    Token.Equal -> Just "="
    _           -> Nothing
  )

-- | Alphanumeric identifiers
alphanumeric :: (MonadParser parser) => parser (Marked Text)
alphanumeric = dbg ["alphanumeric"] . marked . tokenWith $ \case
  Token.Alphanumeric s -> Just s
  _ -> Nothing


-- | Symbolic identifiers
symbolic :: (MonadParser parser) => parser (Marked Text)
symbolic = dbg ["symbolic"] . marked . tokenWith $ \case
  Token.Symbolic s -> Just s
  _ -> Nothing