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
import qualified Text.Megaparsec               as M

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
                                                ( pattern )
import           Parser.Internal.Parsers.Type   ( typ )
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token

-- | Parses a declaration
declaration :: StateT FixityTable Parser Decl
declaration = dbgState ["declaration"] $ do
    -- Handle declaration sequences
  decls <- declaration' `sepBy` lift (token_ Token.Semicolon <|> nothing)
  return $ case decls of
    [decl] -> decl
    _      -> Decl.Sequence decls
 where
  declaration' = choice
    [val, typAlias, datatype, localInEnd, open, nonfix, infixrDecl, infixDecl]

-- Val declarations

val :: StateT FixityTable Parser Decl
val = dbgState ["declaration", "val"] $ do
  lift $ token_ Token.Val
  tyvars   <- lift $ xseq typeVariable
  valbinds <- binds Token.And valBind
  return Decl.Val { Decl.tyvars, Decl.valbinds }

valBind :: Parser () -> StateT FixityTable Parser Decl.ValBind
valBind start = dbgState ["delaration", "val", "valbind"] $ do
  fixityTable <- get
  lift $ do
    start

    maybeRec <- M.observing (token_ Token.Rec)
    let isRec = case maybeRec of
          Left  _  -> False
          Right () -> True

    lhs <- pattern fixityTable
    token_ Token.Equal
    rhs <- expression fixityTable
    return Decl.ValBind { Decl.isRec, Decl.lhs, Decl.rhs }

-- Fun declarations

-- Type aliases
typAlias :: StateT FixityTable Parser Decl
typAlias = do
  lift $ token_ Token.Type
  typbinds <- binds Token.And typBind
  return Decl.TypAlias { Decl.typbinds }

typBind :: Parser () -> StateT FixityTable Parser Decl.TypBind
typBind start = lift $ do
  start
  tyvars <- xseq typeVariable
  tycon  <- typeConstructor
  token_ Token.Equal
  t <- typ
  return Decl.TypBind { Decl.tyvars, Decl.tycon, Decl.typ = t }

-- Datatype declarations/replication

datatype :: StateT FixityTable Parser Decl
datatype = do
  lift $ token_ Token.Datatype
  datbinds <- binds Token.And datBind
  withtype <- optional $ do
    lift $ token_ Token.Withtype
    binds Token.And typBind
  return Decl.Datatype { Decl.datbinds, Decl.withtype }

datBind :: Parser () -> StateT FixityTable Parser Decl.DatBind
datBind start = do
  lift start
  tyvars <- lift $ xseq typeVariable
  tycon  <- lift typeConstructor
  lift $ token_ Token.Equal
  conbinds <- binds Token.Pipe conBind
  return Decl.DatBind { Decl.tyvars, Decl.tycon, Decl.conbinds }

conBind :: Parser () -> StateT FixityTable Parser Decl.ConBind
conBind start = lift $ do
  start
  constructor <- op valueIdentifier
  arg         <- optional $ do
    token_ Token.Of
    typ
  return Decl.ConBind { Decl.constructor, Decl.arg }

-- Abstype declarations

-- Exception declarations

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

  lift $ token_ Token.Local
  decl <- declaration
  lift $ token_ Token.In
  body <- declaration
  lift $ token_ Token.End

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
