module Parser.Internal where

import           Control.Monad.Combinators
                                         hiding ( endBy1
                                                , sepBy1
                                                )
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import           Control.Monad.Combinators.NonEmpty
                                                ( endBy1
                                                , sepBy1
                                                )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Relude.Unsafe                 as Unsafe
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L

import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
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

-- | Parses the toplevel
topLevel :: Parser [Decl]
topLevel = dbg ["topLevel"] $ evalStateT decls FixityTable.basisFixityTable
  where decls = many declaration

-- | Parses a declaration
declaration :: StateT FixityTable Parser Decl
declaration = dbgState ["declaration"]
  $ choice [val, nonfix, infixrDecl, infixDecl]
 where
  val :: StateT FixityTable Parser Decl
  val = dbgState ["declaration", "val"] $ do
    fixityTable <- get
    lift $ do
      reserved Reserved.Val
      lhs <- pattern fixityTable
      reserved Reserved.Equal
      rhs <- expression fixityTable
      return Decl.Val { Decl.lhs, Decl.rhs }

  nonfix :: StateT FixityTable Parser Decl
  nonfix = dbgState ["declaration", "nonfix"] $ do
    ident <- lift $ do
      reserved Reserved.Nonfix
      identifier
    modify $ FixityTable.removeOperator ident
    return Decl.Nonfix { Decl.ident }

  infixrDecl :: StateT FixityTable Parser Decl
  infixrDecl = dbgState ["declaration", "infixrDecl"] $ do
    (precedence, ident) <- lift $ fixityDecl Reserved.Infixr
    modify $ FixityTable.addOperator ident E.InfixR (fromMaybe 0 precedence)
    return Decl.Infixr { Decl.precedence, Decl.ident }

  infixDecl :: StateT FixityTable Parser Decl
  infixDecl = dbgState ["declaration", "infixDecl"] $ do
    (precedence, ident) <- lift $ fixityDecl Reserved.Infix
    modify $ FixityTable.addOperator ident E.InfixL (fromMaybe 0 precedence)
    return Decl.Infix { Decl.precedence, Decl.ident }

  fixityDecl :: Reserved.ReservedWord
             -> Parser (Maybe FixityTable.Precedence, Ident.Untagged)
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
expression fixityTable = dbg ["expression"]
  $ makeFixityParser FixityTable.Expr expression' fixityTable
 where
  expression' = choice
    [ lit
    , var
    -- M.try to prevent failure from consuming the start of a tuple
    , M.try . parens $ expression fixityTable
    , tup
    , lst
    , fn
    , caseof
    ]

  lit = Expr.Lit <$> literal

  -- M.try to prevent failure from trying to parse infix operator as identifier
  var = M.try $ (Expr.Var <$> nonfixValueIdentifier fixityTable)

  tup = Expr.Tuple <$> tuple (expression fixityTable)

  lst = Expr.List <$> list (expression fixityTable)

  fn  = do
    reserved Reserved.Fn
    m <- match fixityTable
    return Expr.Fn { Expr.match = m }

  caseof = do
    reserved Reserved.Case
    expr <- expression fixityTable
    reserved Reserved.Of
    m <- match fixityTable
    return Expr.Case { Expr.expr, Expr.match = m }

match :: FixityTable -> Parser Expr.Match
match fixityTable = matchArm fixityTable `sepBy1` reserved Reserved.Pipe

matchArm :: FixityTable -> Parser Expr.MatchArm
matchArm fixityTable = do
  lhs <- pattern fixityTable
  reserved Reserved.Widearrow
  rhs <- expression fixityTable
  return Expr.MatchArm { Expr.rhs, Expr.lhs }

-- | Parses a pattern
pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg ["pattern"]
  $ makeFixityParser FixityTable.Pat pattern' fixityTable
 where
  pattern' = choice
    [ wild
    , lit
    , var
    -- M.try to prevent failure from consuming the start of a tuple
    , M.try . parens $ pattern fixityTable
    , tup
    , lst
    ]

  wild = dbg ["pattern", "wild"] $ Pat.Wild <$ reserved Reserved.Underscore

  lit  = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- M.try to prevent failure from trying to parse infix operator as identifier
  var  = dbg ["pattern", "var"]
    $ M.try (Pat.Var <$> nonfixValueIdentifier fixityTable)

  tup = Pat.Tuple <$> tuple (pattern fixityTable)

  lst = Pat.List <$> list (pattern fixityTable)

-- | Convenience function that pre-supplies the identifier parser arguments to
--   'FixityTable.makeParser'
makeFixityParser :: FixityTable.Infixable a
                 -> Parser a
                 -> FixityTable
                 -> Parser a
makeFixityParser = FixityTable.makeParser FixityTable.IdentParsers
  { FixityTable.alphanumeric
  , FixityTable.symbolic
  }

-- | @list p@ parses a list, parsing each element with @p@
list :: (Show a) => Parser a -> Parser [a]
list parser = dbg ["list"] $ brackets (parser `sepBy` reserved Reserved.Comma)

-- | @tuple p@ parses a tuple, parsing each element with @p@
tuple :: (Show a) => Parser a -> Parser [a]
tuple parser = dbg ["tuple"] $ parens (parser `sepBy` reserved Reserved.Comma)

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
    M.try $ reserved Reserved.Op
    ValueIdent.Op <$> valueIdentifier

  longIdent = ValueIdent.LongIdent <$> longIdentifier

-- | Parses a possibly qualified identifier
longIdentifier :: Parser LongIdent
longIdentifier = dbg ["longIdentifier"]
  $ choice [qualifiedAlphanum, qualifiedSymbolic, unqualified]
 where
  -- M.try because this could consume a valid bare identifier
  -- | Qualified identifier ending in an alphanumeric identifier
  qualifiedAlphanum :: Parser LongIdent
  qualifiedAlphanum = dbg ["longIdentifier", "qualifiedNonfix"] . M.try $ do
    idents <- alphanumeric `sepBy2` symbol "."
    let ident = Ident.untag $ NonEmpty.last idents
    let qualifiers =
          idents
            |> NonEmpty.take (NonEmpty.length idents - 1)
            |> NonEmpty.nonEmpty
            |> Unsafe.fromJust
    return LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident }

  -- M.try because this could consume a valid bare identifier
  -- | Qualified identifier ending in a symbolic identifier
  qualifiedSymbolic :: Parser LongIdent
  qualifiedSymbolic = dbg ["longIdentifier", "qualifiedInfix"] . M.try $ do
    -- M.try to prevent this from trying and failing to consume the symbolic identifier
    qualifiers <- endBy1 (M.try alphanumeric) (symbol ".")
    ident      <- identifier
    return LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident }

  unqualified :: Parser LongIdent
  unqualified = LongIdent.Ident <$> identifier

-- | Parses a bare identifier
identifier :: Parser Ident.Untagged
identifier = choice [Ident.untag <$> alphanumeric, Ident.untag <$> symbolic]

-- | Alphanumeric identifiers
alphanumeric :: Parser (Ident.Tagged 'Ident.Alphanumeric)
alphanumeric = dbg ["alphanumeric"] . lexeme $ do
  -- The first character must be a letter
  c  <- C.letterChar
  -- The remaining characters can be alphanumeric or underscores
  cs <- M.many (C.alphaNumChar <|> underscore <|> prime)
  let ident = Ident.alphanumeric $ toText (c : cs)
  checkValidIdent ident
  return ident
 where
  underscore = C.char '_'
  prime      = C.char '\''

-- | Symbolic identifiers
symbolic :: Parser (Ident.Tagged 'Ident.Symbolic)
symbolic = dbg ["symbolic"] . lexeme $ do
  cs <- some symbolChar
  let ident = Ident.symbolic $ toText cs
  checkValidIdent ident
  return ident
 where
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

-- | @notReserved ident@ fails iff @ident@ is a valid identifier.
--
--   Valid identifiers are:
--     - a reserved input other than "=" (since "=" is a permitted identifier name).
--
--   @checkValidIdent ident@ consumes no input on success.
checkValidIdent :: (Ident.Identifier i) => i -> Parser ()
checkValidIdent ident = if name `elem` Reserved.reservedTokens && name /= "="
  then fail $ "keyword " ++ show name ++ " cannot be an identifier"
  else return ()
  where name = Ident.name ident

-- | Parses a reserved symbol
reserved :: forall r . Reserved r => r -> Parser ()
reserved res = dbg ["reserved"] . lexeme $ do
  _ <- L.symbol nothing (Reserved.text res)
  M.notFollowedBy $ choice (map C.char (Reserved.disallowedFollowingChars @r))

-- | Parses a numerical literal
literal :: Parser Lit
literal = dbg ["literal"] $ choice
  -- M.try for common prefixes
  [ M.try (Lit.HexWord <$> prefixed "0wx" hexadecimal)
  , M.try (Lit.Hex <$> prefixed "0x" hexadecimal)
  , M.try (Lit.DecWord <$> prefixed "0w" decimal)
  , Lit.Dec <$> decimal
  ]
 where
  prefixed :: Text -> Parser Integer -> Parser Integer
  prefixed prefix integer = symbol prefix >> integer

-- | @braces p@ parses @p@ between braces
braces :: (Show a) => Parser a -> Parser a
braces =
  dbg ["braces"] . between (reserved Reserved.Lbrace) (reserved Reserved.Rbrace)

-- | @brackets p@ parses @p@ between brackets
brackets :: (Show a) => Parser a -> Parser a
brackets = dbg ["brackets"]
  . between (reserved Reserved.Lbracket) (reserved Reserved.Rbracket)

-- | @parens p@ parses @p@ between parentheses
parens :: (Show a) => Parser a -> Parser a
parens =
  dbg ["parens"] . between (reserved Reserved.Lparen) (reserved Reserved.Rparen)
