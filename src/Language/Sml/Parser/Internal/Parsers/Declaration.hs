module Language.Sml.Parser.Internal.Parsers.Declaration
  ( declaration
  )
where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( some )
import qualified Data.List.NonEmpty            as NonEmpty
import           Text.Megaparsec                ( observing
                                                , try
                                                )

import           Language.Sml.Ast.Associativity ( Associativity )
import qualified Language.Sml.Ast.Associativity
                                               as Associativity
import           Language.Sml.Ast.Decl          ( Decl
                                                , MDecl
                                                )
import qualified Language.Sml.Ast.Decl         as Decl
import           Language.Sml.Ast.Ident.ValueIdent
                                                ( MValueIdent )
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.FixityTable
                                                ( FixityTable )
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import           Language.Sml.Parser.Internal.Parsers.Identifier
                                                ( valueIdentifier
                                                , nonfixValueIdentifier
                                                , nonfixLongValueIdentifier
                                                , typeVariable
                                                , typeConstructor
                                                , structureIdentifier
                                                , long
                                                , op
                                                )
import           Language.Sml.Parser.Internal.Parsers.Expression
                                                ( expression )
import           Language.Sml.Parser.Internal.Parsers.Literal
                                                ( decimal
                                                , hexadecimal
                                                )
import           Language.Sml.Parser.Internal.Parsers.Pattern
                                                ( atomicPattern
                                                , pattern
                                                )
import           Language.Sml.Parser.Internal.Parsers.Type
                                                ( typ )
import           Language.Sml.Parser.Internal.Token
                                                ( Token )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token

type FixityMonadParser parser
  = (MonadParser parser, MonadState FixityTable parser)

-- | Parses a declaration
declaration :: (MonadParser parser, MonadState FixityTable parser)
            => parser MDecl
declaration = dbg ["declaration"] $ do
    -- Handle declaration sequences
  decls <- marked $ declaration' `sepBy` (token_ Token.Semicolon <|> nothing)
  case Marked.value decls of
    [decl] -> return decl
    _      -> return (Decl.Sequence <$> decls)
 where
  declaration' = choice
    [ val
    , fun
    , typAlias
    , datatypeReplication
    , datatype
    , abstype
    , exception
    , localInEnd
    , open
    , nonfix
    , infixrDecl
    , infixDecl
    ]

-- Val declarations

val :: (FixityMonadParser parser) => parser MDecl
val = dbg ["declaration", "val"] . marked $ do
  token_ Token.Val
  tyvars   <- xseq typeVariable
  valbinds <- binds Token.And valBind
  return Decl.Val { Decl.tyvars, Decl.valbinds }

valBind :: (FixityMonadParser parser) => Parser () -> parser Decl.MValBind
valBind start = dbg ["delaration", "val", "valbind"] . marked $ do
  fixityTable <- get

  liftParser start

  maybeRec <- observing (token_ Token.Rec)
  let isRec = case maybeRec of
        Left  _  -> False
        Right () -> True

  lhs <- pattern fixityTable
  token_ Token.Equal
  rhs <- expression fixityTable
  return Decl.ValBind { Decl.isRec, Decl.lhs, Decl.rhs }

-- Fun declarations

fun :: (FixityMonadParser parser) => parser MDecl
fun = dbg ["declaration", "fun"] . marked $ do
  token_ Token.Fun
  tyvars   <- xseq typeVariable
  funbinds <- binds Token.And funBind
  return Decl.Fun { Decl.tyvars, Decl.funbinds }

funBind :: (FixityMonadParser parser) => Parser () -> parser Decl.MFunBind
funBind start = dbg ["declaration", "fun", "funBind"] . marked $ do
  liftParser start
  clauses <- binds Token.Pipe funClause
  return Decl.FunBind { Decl.clauses }

funClause :: (FixityMonadParser parser) => Parser () -> parser Decl.FunClause
funClause start = dbg ["declaration", "fun", "funBind", "funClause"] $ do
  liftParser start
  choice [infixClause, nonfixClause]
 where
  -- TODO(tkadur) enforce parens as the standard requires
  -- @try@ to prevent conflict with nonfix clause
  infixClause = try $ do
    fixityTable <- get
    lhs         <- atomicPattern fixityTable
    infixName   <- valueIdentifier
    rhs         <- atomicPattern fixityTable
    infixArgs   <- many (atomicPattern fixityTable)
    returnTyp   <- optional (token_ Token.Colon >> typ)
    token_ Token.Equal
    body <- expression fixityTable
    return Decl.InfixClause { Decl.lhs
                            , Decl.infixName
                            , Decl.rhs
                            , Decl.infixArgs
                            , Decl.returnTyp
                            , Decl.body
                            }

  nonfixClause = do
    fixityTable <- get
    nonfixName  <- nonfixValueIdentifier fixityTable
    nonfixArgs  <- some (atomicPattern fixityTable)
    returnTyp   <- optional (token_ Token.Colon >> typ)
    token_ Token.Equal
    body <- expression fixityTable
    return Decl.NonfixClause { Decl.nonfixName
                             , Decl.nonfixArgs
                             , Decl.returnTyp
                             , Decl.body
                             }

-- Type aliases

typAlias :: (FixityMonadParser parser) => parser MDecl
typAlias = dbg ["declaration", "typAlias"] . marked $ do
  token_ Token.Type
  typbinds <- binds Token.And typBind
  return Decl.TypAlias { Decl.typbinds }

typBind :: (FixityMonadParser parser) => Parser () -> parser Decl.MTypBind
typBind start = dbg ["declaration", "typAlias", "typBind"] . marked $ do
  liftParser start
  tyvars <- xseq typeVariable
  tycon  <- typeConstructor
  token_ Token.Equal
  t <- typ
  return Decl.TypBind { Decl.tyvars, Decl.tycon, Decl.typ = t }

-- Datatype replication

datatypeReplication :: (FixityMonadParser parser) => parser MDecl
datatypeReplication = dbg ["declaration", "datatypeReplication"] . marked $ do
  -- @try@ to prevent conflict with regular datatype declarations
  new <- try $ do
    token_ Token.Datatype
    new <- typeConstructor
    token_ Token.Equal
    token_ Token.Datatype
    return new
  old <- long typeConstructor
  return Decl.DatatypeReplication { Decl.new, Decl.old }

-- Datatype declaration

datatype :: (FixityMonadParser parser) => parser MDecl
datatype = dbg ["declaration", "datatype"] . marked $ do
  token_ Token.Datatype
  datbinds <- binds Token.And datBind
  withtype <- optional $ do
    token_ Token.Withtype
    binds Token.And typBind
  return Decl.Datatype { Decl.datbinds, Decl.withtype }

datBind :: (FixityMonadParser parser) => Parser () -> parser Decl.MDatBind
datBind start = dbg ["declaration", "datatype", "datBind"] . marked $ do
  liftParser start
  tyvars <- xseq typeVariable
  tycon  <- typeConstructor
  token_ Token.Equal
  conbinds <- binds Token.Pipe conBind
  return Decl.DatBind { Decl.tyvars, Decl.tycon, Decl.conbinds }

conBind :: (FixityMonadParser parser) => Parser () -> parser Decl.ConBind
conBind start = dbg ["declaration", "datatype", "datBind", "conBind"] $ do
  liftParser start
  constructor <- op valueIdentifier
  arg         <- optional (token_ Token.Of >> typ)
  return Decl.ConBind { Decl.constructor, Decl.arg }

-- Abstype declarations
abstype :: (FixityMonadParser parser) => parser MDecl
abstype = dbg ["declaration", "abstype"] . marked $ do
  token_ Token.Abstype
  datbinds <- binds Token.And datBind
  withtype <- optional $ do
    token_ Token.Withtype
    binds Token.And typBind
  token_ Token.With
  decl <- declaration
  token_ Token.End
  return Decl.Abstype { Decl.datbinds, Decl.withtype, Decl.decl }

-- Exception declarations
exception :: (FixityMonadParser parser) => parser MDecl
exception = dbg ["declaration", "exception"] . marked $ do
  token_ Token.Exception
  exnbinds <- binds Token.And exnBind
  return Decl.Exception { Decl.exnbinds }

exnBind :: forall parser
         . (FixityMonadParser parser)
        => Parser ()
        -> parser Decl.MExnBind
exnBind start = dbg ["declaration", "exception", "exnBind"] . marked $ do
  liftParser start
  choice [replication, regular]
 where
  replication = do
    fixityTable <- get
    -- @try@ to prevent conflict with regular exnbinds
    new         <- try (nonfixValueIdentifier fixityTable << token_ Token.Equal)
    old         <- nonfixLongValueIdentifier fixityTable
    return Decl.ExnReplication { Decl.new, Decl.old }

  regular = do
    fixityTable <- get
    constructor <- nonfixValueIdentifier fixityTable
    arg         <- optional (token_ Token.Of >> typ)
    return Decl.ExnBind { Decl.constructor, Decl.arg }

-- Common functionality for chained bindings

binds :: (FixityMonadParser parser)
      => Token
      -> (Parser () -> parser a)
      -> parser (NonEmpty a)
binds separator bind = do
  firstBind <- bind nothing
  andBinds  <- many $ bind (token_ separator)
  return $ firstBind :| andBinds

-- Local blocks

localInEnd :: (FixityMonadParser parser) => parser MDecl
localInEnd = dbg ["declaration", "localInEnd"] . marked $ do
  -- Store old fixity table
  fixityTable <- get

  token_ Token.Local
  decl <- declaration
  token_ Token.In
  body <- declaration
  token_ Token.End

  -- Reset fixity table, then scan body for fixity decls and update it
  -- TODO(tkadur) This is a terrible hack - find a better way
  put fixityTable
  applyFixityDecl (Marked.value body)

  return Decl.Local { Decl.decl, Decl.body }
 where
  applyFixityDecl :: (FixityMonadParser parser) => Decl -> parser ()
  applyFixityDecl = \case
    Decl.Infix { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Left
    Decl.Infixr { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Right
    Decl.Nonfix { Decl.idents } ->
      modify $ FixityTable.removeOperators (NonEmpty.map Marked.value idents)
    Decl.Sequence decls -> mapM_ applyFixityDecl (map Marked.value decls)
    _ -> return ()

-- Open directives

open :: (FixityMonadParser parser) => parser MDecl
open = dbg ["declaration", "open"] . marked $ do
  token_ Token.Open
  stridents <- some (long structureIdentifier)
  return $ Decl.Open stridents

-- Fixity declarations

nonfix :: (FixityMonadParser parser) => parser MDecl
nonfix = dbg ["declaration", "nonfix"] . marked $ do
  token_ Token.Nonfix
  idents <- some valueIdentifier
  modify $ FixityTable.removeOperators (NonEmpty.map Marked.value idents)
  return Decl.Nonfix { Decl.idents }

infixrDecl :: (FixityMonadParser parser) => parser MDecl
infixrDecl = dbg ["declaration", "infixrDecl"] . marked $ do
  (precedence, idents) <- fixityDecl Token.Infixr
  addToFixityTable precedence idents Associativity.Right
  return Decl.Infixr { Decl.precedence, Decl.idents }

infixDecl :: (FixityMonadParser parser) => parser MDecl
infixDecl = dbg ["declaration", "infixDecl"] . marked $ do
  (precedence, idents) <- fixityDecl Token.Infix
  addToFixityTable precedence idents Associativity.Left
  return Decl.Infix { Decl.precedence, Decl.idents }

addToFixityTable :: (FixityMonadParser parser)
                 => Maybe Int
                 -> NonEmpty MValueIdent
                 -> Associativity
                 -> parser ()
addToFixityTable precedence idents associativity =
  modify $ FixityTable.addOperators (NonEmpty.map Marked.value idents)
                                    associativity
                                    (fromMaybe 0 precedence)

fixityDecl :: (MonadParser parser)
           => Token
           -> parser (Maybe FixityTable.Precedence, NonEmpty MValueIdent)
fixityDecl keyword = do
  token_ keyword
  precedence <- optional integer
  ident      <- some valueIdentifier
  -- Precedence must either be unspecified or within [0, 9]
  if not $ precedence `elem` (Nothing : map Just [0 .. 9])
    then fail "fixity precedence must be between 0 and 9"
    else return (fromIntegral <$> precedence, ident)
 where
  -- | Parses an integer literal (in any base)
  integer :: (MonadParser parser) => parser Integer
  integer = decimal <|> hexadecimal
