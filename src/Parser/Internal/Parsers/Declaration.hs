module Parser.Internal.Parsers.Declaration
  ( declaration
  )
where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( some )
import           Text.Megaparsec                ( observing
                                                , try
                                                )

import           Ast.Associativity              ( Associativity )
import qualified Ast.Associativity             as Associativity
import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( valueIdentifier
                                                , nonfixValueIdentifier
                                                , nonfixLongValueIdentifier
                                                , typeVariable
                                                , typeConstructor
                                                , structureIdentifier
                                                , long
                                                , op
                                                )
import           Parser.Internal.Parsers.Expression
                                                ( expression )
import           Parser.Internal.Parsers.Literal
                                                ( decimal
                                                , hexadecimal
                                                )
import           Parser.Internal.Parsers.Pattern
                                                ( atomicPattern
                                                , pattern
                                                )
import           Parser.Internal.Parsers.Type   ( typ )
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token

type FixityMonadParser parser
  = (MonadParser parser, MonadState FixityTable parser)

-- | Parses a declaration
declaration :: (MonadParser parser, MonadState FixityTable parser)
            => parser Decl
declaration = dbg ["declaration"] $ do
    -- Handle declaration sequences
  decls <- declaration' `sepBy` (token_ Token.Semicolon <|> nothing)
  return $ case decls of
    [decl] -> decl
    _      -> Decl.Sequence decls
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

val :: (FixityMonadParser parser) => parser Decl
val = dbg ["declaration", "val"] $ do
  token_ Token.Val
  tyvars   <- xseq typeVariable
  valbinds <- binds Token.And valBind
  return Decl.Val { Decl.tyvars, Decl.valbinds }

valBind :: (FixityMonadParser parser) => Parser () -> parser Decl.ValBind
valBind start = dbg ["delaration", "val", "valbind"] $ do
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

fun :: (FixityMonadParser parser) => parser Decl
fun = dbg ["declaration", "fun"] $ do
  token_ Token.Fun
  tyvars   <- xseq typeVariable
  funbinds <- binds Token.And funBind
  return Decl.Fun { Decl.tyvars, Decl.funbinds }

funBind :: (FixityMonadParser parser) => Parser () -> parser Decl.FunBind
funBind start = dbg ["declaration", "fun", "funBind"] $ do
  liftParser start
  clauses <- binds Token.Pipe funClause
  return Decl.FunBind { Decl.clauses }

funClause :: (FixityMonadParser parser) => Parser () -> parser Decl.FunClause
funClause start = dbg ["declaration", "fun", "funBind", "funClause"] $ do
  liftParser start
  choice [nonfixClause, infixClause]
 where
  nonfixClause = do
    fixityTable <- get
    nonfixName  <- nonfixValueIdentifier fixityTable
    nonfixArgs  <- some (atomicPattern fixityTable)
    returnType  <- optional (token_ Token.Colon >> typ)
    token_ Token.Equal
    body <- expression fixityTable
    return Decl.NonfixClause { Decl.nonfixName
                             , Decl.nonfixArgs
                             , Decl.returnType
                             , Decl.body
                             }

  -- TODO(tkadur) enforce parens as the standard requires
  infixClause = do
    fixityTable <- get
    lhs         <- atomicPattern fixityTable
    infixName   <- valueIdentifier
    rhs         <- atomicPattern fixityTable
    infixArgs   <- many (atomicPattern fixityTable)
    returnType  <- optional (token_ Token.Colon >> typ)
    token_ Token.Equal
    body <- expression fixityTable
    return Decl.InfixClause { Decl.lhs
                            , Decl.infixName
                            , Decl.rhs
                            , Decl.infixArgs
                            , Decl.returnType
                            , Decl.body
                            }

-- Type aliases

typAlias :: (FixityMonadParser parser) => parser Decl
typAlias = dbg ["declaration", "typAlias"] $ do
  token_ Token.Type
  typbinds <- binds Token.And typBind
  return Decl.TypAlias { Decl.typbinds }

typBind :: (FixityMonadParser parser) => Parser () -> parser Decl.TypBind
typBind start = dbg ["declaration", "typAlias", "typBind"] $ do
  liftParser start
  tyvars <- xseq typeVariable
  tycon  <- typeConstructor
  token_ Token.Equal
  t <- typ
  return Decl.TypBind { Decl.tyvars, Decl.tycon, Decl.typ = t }

-- Datatype replication

datatypeReplication :: (FixityMonadParser parser) => parser Decl
datatypeReplication = dbg ["declaration", "datatypeReplication"] $ do
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

datatype :: (FixityMonadParser parser) => parser Decl
datatype = dbg ["declaration", "datatype"] $ do
  token_ Token.Datatype
  datbinds <- binds Token.And datBind
  withtype <- optional $ do
    token_ Token.Withtype
    binds Token.And typBind
  return Decl.Datatype { Decl.datbinds, Decl.withtype }

datBind :: (FixityMonadParser parser) => Parser () -> parser Decl.DatBind
datBind start = dbg ["declaration", "datatype", "datBind"] $ do
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
abstype :: (FixityMonadParser parser) => parser Decl
abstype = dbg ["declaration", "abstype"] $ do
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
exception :: (FixityMonadParser parser) => parser Decl
exception = dbg ["declaration", "exception"] $ do
  token_ Token.Exception
  exnbinds <- binds Token.And exnBind
  return Decl.Exception { Decl.exnbinds }

exnBind :: forall parser
         . (FixityMonadParser parser)
        => Parser ()
        -> parser Decl.ExnBind
exnBind start = dbg ["declaration", "exception", "exnBind"] $ do
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

localInEnd :: (FixityMonadParser parser) => parser Decl
localInEnd = dbg ["declaration", "localInEnd"] $ do
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
  applyFixityDecl body

  return Decl.Local { Decl.decl, Decl.body }
 where
  applyFixityDecl :: (FixityMonadParser parser) => Decl -> parser ()
  applyFixityDecl = \case
    Decl.Infix { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Left
    Decl.Infixr { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Right
    Decl.Nonfix { Decl.idents } -> modify $ FixityTable.removeOperators idents
    Decl.Sequence decls -> mapM_ applyFixityDecl decls
    _ -> return ()

-- Open directives

open :: (FixityMonadParser parser) => parser Decl
open = dbg ["declaration", "open"] $ do
  token_ Token.Open
  stridents <- some (long structureIdentifier)
  return $ Decl.Open stridents

-- Fixity declarations

nonfix :: (FixityMonadParser parser) => parser Decl
nonfix = dbg ["declaration", "nonfix"] $ do
  token_ Token.Nonfix
  idents <- some valueIdentifier
  modify $ FixityTable.removeOperators idents
  return Decl.Nonfix { Decl.idents }

infixrDecl :: (FixityMonadParser parser) => parser Decl
infixrDecl = dbg ["declaration", "infixrDecl"] $ do
  (precedence, idents) <- fixityDecl Token.Infixr
  addToFixityTable precedence idents Associativity.Right
  return Decl.Infixr { Decl.precedence, Decl.idents }

infixDecl :: (FixityMonadParser parser) => parser Decl
infixDecl = dbg ["declaration", "infixDecl"] $ do
  (precedence, idents) <- fixityDecl Token.Infix
  addToFixityTable precedence idents Associativity.Left
  return Decl.Infix { Decl.precedence, Decl.idents }

addToFixityTable :: (FixityMonadParser parser)
                 => Maybe Int
                 -> NonEmpty ValueIdent
                 -> Associativity
                 -> parser ()
addToFixityTable precedence idents associativity =
  modify
    $ FixityTable.addOperators idents associativity (fromMaybe 0 precedence)

fixityDecl :: (MonadParser parser)
           => Token
           -> parser (Maybe FixityTable.Precedence, NonEmpty ValueIdent)
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
