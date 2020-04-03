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

-- | Parses a declaration
declaration :: StateT FixityTable Parser Decl
declaration = dbgState ["declaration"] $ do
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

val :: StateT FixityTable Parser Decl
val = dbgState ["declaration", "val"] $ do
  token_ Token.Val
  tyvars   <- lift $ xseq typeVariable
  valbinds <- binds Token.And valBind
  return Decl.Val { Decl.tyvars, Decl.valbinds }

valBind :: Parser () -> StateT FixityTable Parser Decl.ValBind
valBind start = dbgState ["delaration", "val", "valbind"] $ do
  fixityTable <- get
  lift $ do
    start

    maybeRec <- observing (token_ Token.Rec)
    let isRec = case maybeRec of
          Left  _  -> False
          Right () -> True

    lhs <- pattern fixityTable
    token_ Token.Equal
    rhs <- expression fixityTable
    return Decl.ValBind { Decl.isRec, Decl.lhs, Decl.rhs }

-- Fun declarations

fun :: StateT FixityTable Parser Decl
fun = dbgState ["declaration", "fun"] $ do
  token_ Token.Fun
  tyvars   <- lift $ xseq typeVariable
  funbinds <- binds Token.And funBind
  return Decl.Fun { Decl.tyvars, Decl.funbinds }

funBind :: Parser () -> StateT FixityTable Parser Decl.FunBind
funBind start = dbgState ["declaration", "fun", "funBind"] $ do
  lift start
  clauses <- binds Token.Pipe funClause
  return Decl.FunBind { Decl.clauses }

funClause :: Parser () -> StateT FixityTable Parser Decl.FunClause
funClause start = dbgState ["declaration", "fun", "funBind", "funClause"] $ do
  lift start
  choice [nonfixClause, infixClause]
 where
  nonfixClause = do
    fixityTable <- get
    lift $ do
      nonfixName <- nonfixValueIdentifier fixityTable
      nonfixArgs <- some (atomicPattern fixityTable)
      returnType <- optional (token_ Token.Colon >> typ)
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
    lift $ do
      lhs        <- atomicPattern fixityTable
      infixName  <- valueIdentifier
      rhs        <- atomicPattern fixityTable
      infixArgs  <- many (atomicPattern fixityTable)
      returnType <- optional (token_ Token.Colon >> typ)
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

typAlias :: StateT FixityTable Parser Decl
typAlias = dbgState ["declaration", "typAlias"] $ do
  token_ Token.Type
  typbinds <- binds Token.And typBind
  return Decl.TypAlias { Decl.typbinds }

typBind :: Parser () -> StateT FixityTable Parser Decl.TypBind
typBind start = dbgState ["declaration", "typAlias", "typBind"] . lift $ do
  start
  tyvars <- xseq typeVariable
  tycon  <- typeConstructor
  token_ Token.Equal
  t <- typ
  return Decl.TypBind { Decl.tyvars, Decl.tycon, Decl.typ = t }

-- Datatype replication

datatypeReplication :: StateT FixityTable Parser Decl
datatypeReplication =
  dbgState ["declaration", "datatypeReplication"] . lift $ do
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

datatype :: StateT FixityTable Parser Decl
datatype = dbgState ["declaration", "datatype"] $ do
  token_ Token.Datatype
  datbinds <- binds Token.And datBind
  withtype <- optional $ do
    token_ Token.Withtype
    binds Token.And typBind
  return Decl.Datatype { Decl.datbinds, Decl.withtype }

datBind :: Parser () -> StateT FixityTable Parser Decl.DatBind
datBind start = dbgState ["declaration", "datatype", "datBind"] $ do
  lift start
  tyvars <- lift $ xseq typeVariable
  tycon  <- lift typeConstructor
  token_ Token.Equal
  conbinds <- binds Token.Pipe conBind
  return Decl.DatBind { Decl.tyvars, Decl.tycon, Decl.conbinds }

conBind :: Parser () -> StateT FixityTable Parser Decl.ConBind
conBind start =
  dbgState ["declaration", "datatype", "datBind", "conBind"] . lift $ do
    start
    constructor <- op valueIdentifier
    arg         <- optional (token_ Token.Of >> typ)
    return Decl.ConBind { Decl.constructor, Decl.arg }

-- Abstype declarations
abstype :: StateT FixityTable Parser Decl
abstype = dbgState ["declaration", "abstype"] $ do
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
exception :: StateT FixityTable Parser Decl
exception = dbgState ["declaration", "exception"] $ do
  token_ Token.Exception
  exnbinds <- binds Token.And exnBind
  return Decl.Exception { Decl.exnbinds }

exnBind :: Parser () -> StateT FixityTable Parser Decl.ExnBind
exnBind start = dbgState ["declaration", "exception", "exnBind"] $ do
  lift start
  choice [replication, regular]
 where
  replication = do
    fixityTable <- get
    lift $ do
      -- @try@ to prevent conflict with regular exnbinds
      new <- try (nonfixValueIdentifier fixityTable << token_ Token.Equal)
      old <- nonfixLongValueIdentifier fixityTable
      return Decl.ExnReplication { Decl.new, Decl.old }

  regular = do
    fixityTable <- get
    lift $ do
      constructor <- nonfixValueIdentifier fixityTable
      arg         <- optional (token_ Token.Of >> typ)
      return Decl.ExnBind { Decl.constructor, Decl.arg }

-- Common functionality for chained bindings

binds :: Token
      -> (Parser () -> StateT FixityTable Parser a)
      -> StateT FixityTable Parser (NonEmpty a)
binds separator bind = do
  firstBind <- bind nothing
  andBinds  <- many $ bind (token_ separator)
  return $ firstBind :| andBinds

-- Local blocks

localInEnd :: StateT FixityTable Parser Decl
localInEnd = dbgState ["declaration", "localInEnd"] $ do
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
  applyFixityDecl :: Decl -> StateT FixityTable Parser ()
  applyFixityDecl = \case
    Decl.Infix { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Left
    Decl.Infixr { Decl.precedence, Decl.idents } ->
      addToFixityTable precedence idents Associativity.Right
    Decl.Nonfix { Decl.idents } -> modify $ FixityTable.removeOperators idents
    Decl.Sequence decls -> mapM_ applyFixityDecl decls
    _ -> return ()

-- Open directives

open :: StateT FixityTable Parser Decl
open = dbgState ["declaration", "open"] . lift $ do
  token_ Token.Open
  stridents <- some (long structureIdentifier)
  return $ Decl.Open stridents

-- Fixity declarations

nonfix :: StateT FixityTable Parser Decl
nonfix = dbgState ["declaration", "nonfix"] $ do
  idents <- lift $ do
    token_ Token.Nonfix
    some valueIdentifier
  modify $ FixityTable.removeOperators idents
  return Decl.Nonfix { Decl.idents }

infixrDecl :: StateT FixityTable Parser Decl
infixrDecl = dbgState ["declaration", "infixrDecl"] $ do
  (precedence, idents) <- lift $ fixityDecl Token.Infixr
  addToFixityTable precedence idents Associativity.Right
  return Decl.Infixr { Decl.precedence, Decl.idents }

infixDecl :: StateT FixityTable Parser Decl
infixDecl = dbgState ["declaration", "infixDecl"] $ do
  (precedence, idents) <- lift $ fixityDecl Token.Infix
  addToFixityTable precedence idents Associativity.Left
  return Decl.Infix { Decl.precedence, Decl.idents }

addToFixityTable :: Maybe Int
                 -> NonEmpty ValueIdent
                 -> Associativity
                 -> StateT FixityTable Parser ()
addToFixityTable precedence idents associativity =
  modify
    $ FixityTable.addOperators idents associativity (fromMaybe 0 precedence)

fixityDecl :: Token
           -> Parser (Maybe FixityTable.Precedence, NonEmpty ValueIdent)
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
  integer :: Parser Integer
  integer = decimal <|> hexadecimal
