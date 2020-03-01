module Parser.Internal where

import           Control.Monad.Combinators
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L

import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Ident.Ident                ( Ident )
import qualified Ast.Ident.Ident               as Ident
import           Ast.Ident.LongIdent            ( LongIdent )
import qualified Ast.Ident.LongIdent           as LongIdent
import           Ast.Ident.ValueIdent           ( ValueIdent )
import qualified Ast.Ident.ValueIdent          as ValueIdent
import           Ast.Lit                        ( Lit )
import qualified Ast.Lit                       as Lit
import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import           Parser.Internal.Basic
import           Parser.Internal.Combinators
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Reserved       ( Reserved )
import qualified Parser.Internal.Reserved      as Reserved

data AllowedFixity
  = AnyFixity
  | NonfixOnly

-- | Parses the toplevel
topLevel :: Parser [Decl]
topLevel = dbg "topLevel" $ evalStateT decls FixityTable.basisFixityTable
  where decls = many declaration

-- | Parses a declaration
declaration :: StateT FixityTable Parser Decl
declaration = dbgState "declaration"
  $ choice [val, nonfix, infixrDecl, infixDecl]
 where
  val :: StateT FixityTable Parser Decl
  val = dbgState "val" $ do
    fixityTable <- get
    lift $ do
      reserved Reserved.Val
      lhs <- pattern fixityTable
      reserved Reserved.Equal
      rhs <- expression fixityTable
      return $ Decl.Val { Decl.lhs, Decl.rhs }

  nonfix :: StateT FixityTable Parser Decl
  nonfix = dbgState "nonfix" $ do
    ident <- lift $ do
      reserved Reserved.Nonfix
      identifier
    modify $ FixityTable.removeOperator ident
    return $ Decl.Nonfix { Decl.ident }

  infixrDecl :: StateT FixityTable Parser Decl
  infixrDecl = dbgState "infixrDecl" $ do
    (precedence, ident) <- lift $ fixityDecl Reserved.Infixr
    modify $ FixityTable.addOperator ident E.InfixR (fromMaybe 0 precedence)
    return $ Decl.Infixr { Decl.precedence, Decl.ident }

  infixDecl :: StateT FixityTable Parser Decl
  infixDecl = dbgState "infixDecl" $ do
    (precedence, ident) <- lift $ fixityDecl Reserved.Infix
    modify $ FixityTable.addOperator ident E.InfixL (fromMaybe 0 precedence)
    return $ Decl.Infix { Decl.precedence, Decl.ident }

  fixityDecl :: Reserved.ReservedWord
             -> Parser (Maybe FixityTable.Precedence, Ident)
  fixityDecl keyword = do
    -- M.try to avoid "infix" and "infixr" clashing
    M.try $ reserved keyword
    precedence <- M.optional decimal
    ident      <- identifier
    -- Precedence must either be unspecified or within [0, 9]
    if not $ precedence `elem` (Nothing : map Just [0 .. 9])
      then fail "fixity precedence must be between 0 and 9"
      else return (fromIntegral <$> precedence, ident)

-- | Parses an expression
expression :: FixityTable -> Parser Expr
expression fixityTable = dbg "expression"
  $ FixityTable.makeParser FixityTable.Expr expression' fixityTable
 where
  expression' = choice
    [ lit
    , var
    -- M.try to prevent failure from consuming the start of a tuple
    , M.try . parens $ expression fixityTable
    , tup
    ]

  lit = Expr.Lit <$> literal

  -- M.try to prevent failure from trying to parse infix operator as identifier
  var =
    M.try $ runReaderT (Expr.Var <$> valueIdentifier NonfixOnly) fixityTable

  tup = Expr.Tuple <$> tuple (expression fixityTable)

-- | Parses a pattern
pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg "pattern"
  $ FixityTable.makeParser FixityTable.Pat pattern' fixityTable
 where
  pattern' = choice
    [ wild
    , lit
    , var
    -- M.try to prevent failure from consuming the start of a tuple
    , M.try . parens $ pattern fixityTable
    , tup
    ]

  wild = Pat.Wild <$ reserved Reserved.Underscore

  lit  = Pat.Lit <$> literal

  -- M.try to prevent failure from trying to parse infix operator as identifier
  var  = M.try (Pat.Var <$> runReaderT (valueIdentifier NonfixOnly) fixityTable)

  tup  = Pat.Tuple <$> tuple (pattern fixityTable)

-- | @tuple p@ parses a tuple, parsing each element with @p@
tuple :: (Show a) => Parser a -> Parser [a]
tuple parser = dbg "tuple" $ parens (parser `sepBy` reserved Reserved.Comma)

-- | Parses a possibly qualified, possible "op"-prefixed identifier
valueIdentifier :: AllowedFixity -> ReaderT FixityTable Parser ValueIdent
valueIdentifier allowedFixity = dbgReader "valueIdentifier"
  $ choice [op, longIdent]
 where
  op = do
    -- M.try in case a longIdent starts with "op"
    M.try . lift $ reserved Reserved.Op
    ValueIdent.Op <$> valueIdentifier AnyFixity

  longIdent = ValueIdent.LongIdent <$> longIdentifier allowedFixity

-- | Parses a possibly qualified identifier
longIdentifier :: AllowedFixity -> ReaderT FixityTable Parser LongIdent
longIdentifier allowedFixity = dbgReader "longIdentifier" $ choice
  [qualifiedNonfix, qualifiedInfix, LongIdent.Ident <$> ident allowedFixity]
 where
  -- M.try because this could consume a valid bare identifier
  -- | Qualified identifier ending in a nonfixed identifier
  qualifiedNonfix :: ReaderT FixityTable Parser LongIdent
  qualifiedNonfix = dbgReader "qualifiedNonfix" . M.try $ do
    idents <- sepBy2 (ident NonfixOnly) (lift $ symbol ".")
    let x          = NonEmpty.last idents
    let qualifiers = NonEmpty.take (NonEmpty.length idents - 1) idents
    return $ LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident = x }

  -- M.try because this could consume a valid bare identifier
  -- | Qualified identifier ending in a infixed identifier
  qualifiedInfix :: ReaderT FixityTable Parser LongIdent
  qualifiedInfix = dbgReader "qualifiedInfix" . M.try $ do
    -- M.try to prevent this from trying and failing to consume the infix identifier
    qualifiers <- endBy1 (M.try $ ident NonfixOnly) (lift $ symbol ".")
    x          <- ident AnyFixity
    return $ LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident = x }

  -- | Bare identifier
  ident :: AllowedFixity -> ReaderT FixityTable Parser Ident
  ident fixityAllowed = dbgReader "ident" $ do
    operators <- FixityTable.operators <$> ask
    x         <- lift identifier
    case fixityAllowed of
      AnyFixity  -> return x
      NonfixOnly -> if x `HashSet.member` operators
        then fail $ "unexpected infix identifier " ++ show x
        else return x

-- | Parses a bare identifier
identifier :: Parser Ident
identifier = dbg "identifier" . lexeme $ do
  ident <- toText <$> (alphanumeric <|> symbolic)
  -- Make sure the identifier isn't a reserved keyword (other than =)
  if ident `elem` Reserved.reservedTokens && ident /= "="
    then fail $ "keyword " ++ show ident ++ " cannot be an identifier"
    else return $ Ident.Ident ident
 where
  -- | Alphanumeric identifiers
  alphanumeric =
    (:)
      -- The first character must be a letter
      <$> C.letterChar
      -- The remaining characters can be alphanumeric or underscores
      <*> M.many (C.alphaNumChar <|> underscore <|> prime)
  underscore = C.char '_'
  prime      = C.char '\''

  -- | Symbolic identifiers
  symbolic   = some symbolChar
  symbolChar = choice $ map
    C.char
    [ '!'
    , '%'
    , '&'
    , '$'
    , '#'
    , '+'
    , '-'
    , '/'
    , ':'
    , '<'
    , '='
    , '>'
    , '?'
    , '@'
    , '\\'
    , '~'
    , '`'
    , '^'
    , '|'
    , '*'
    ]

-- | Parses a reserved symbol
reserved :: forall r . Reserved r => r -> Parser ()
reserved res = dbg "reserved" . lexeme $ do
  _ <- L.symbol nothing (Reserved.text res)
  M.notFollowedBy . choice $ map C.char (Reserved.disallowedFollowingChars @r)

-- | Parses a numerical literal
literal :: Parser Lit
literal = dbg "literal" $ choice
  -- M.try for common prefixes
  [ M.try (Lit.HexWord <$> prefixed "0wx" hexadecimal)
  , M.try (Lit.Hex <$> prefixed "0x" hexadecimal)
  , M.try (Lit.DecWord <$> prefixed "0w" decimal)
  , Lit.Dec <$> decimal
  ]
 where
  prefixed :: Text -> Parser Integer -> Parser Integer
  prefixed prefix integer = symbol prefix >> integer

-- | @parens p@ parses @p@ between parentheses
parens :: (Show a) => Parser a -> Parser a
parens =
  dbg "parens" . between (reserved Reserved.Lparen) (reserved Reserved.Rparen)
