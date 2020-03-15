module Parser.Internal where

import           Control.Monad.Combinators
                                         hiding ( endBy1
                                                , sepBy1
                                                )
import qualified Control.Monad.Combinators.Expr
                                               as E
import           Control.Monad.Combinators.NonEmpty
                                                ( endBy1
                                                , sepBy1
                                                )
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Scientific                ( Scientific )
import qualified Relude.Unsafe                 as Unsafe
import qualified Text.Megaparsec               as M

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
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token

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
      token_ Token.Val
      lhs <- pattern fixityTable
      token_ Token.Equal
      rhs <- expression fixityTable
      return Decl.Val { Decl.lhs, Decl.rhs }

  nonfix :: StateT FixityTable Parser Decl
  nonfix = dbgState ["declaration", "nonfix"] $ do
    ident <- lift $ do
      token_ Token.Nonfix
      identifier
    modify $ FixityTable.removeOperator ident
    return Decl.Nonfix { Decl.ident }

  infixrDecl :: StateT FixityTable Parser Decl
  infixrDecl = dbgState ["declaration", "infixrDecl"] $ do
    (precedence, ident) <- lift $ fixityDecl Token.Infixr
    modify $ FixityTable.addOperator ident E.InfixR (fromMaybe 0 precedence)
    return Decl.Infixr { Decl.precedence, Decl.ident }

  infixDecl :: StateT FixityTable Parser Decl
  infixDecl = dbgState ["declaration", "infixDecl"] $ do
    (precedence, ident) <- lift $ fixityDecl Token.Infix
    modify $ FixityTable.addOperator ident E.InfixL (fromMaybe 0 precedence)
    return Decl.Infix { Decl.precedence, Decl.ident }

  fixityDecl :: Token -> Parser (Maybe FixityTable.Precedence, Ident)
  fixityDecl keyword = do
    token_ keyword
    precedence <- M.optional integer
    ident      <- identifier
    -- Precedence must either be unspecified or within [0, 9]
    if not $ precedence `elem` (Nothing : map Just [0 .. 9])
      then fail "fixity precedence must be between 0 and 9"
      else return (fromIntegral <$> precedence, ident)

-- | Parses an expression
expression :: FixityTable -> Parser Expr
expression fixityTable = dbg ["expression"]
  $ FixityTable.makeParser FixityTable.Expr expression' fixityTable
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
    token_ Token.Fn
    m <- match fixityTable
    return Expr.Fn { Expr.match = m }

  caseof = do
    token_ Token.Case
    expr <- expression fixityTable
    token_ Token.Of
    m <- match fixityTable
    return Expr.Case { Expr.expr, Expr.match = m }

match :: FixityTable -> Parser Expr.Match
match fixityTable = matchArm fixityTable `sepBy1` token_ Token.Pipe

matchArm :: FixityTable -> Parser Expr.MatchArm
matchArm fixityTable = do
  lhs <- pattern fixityTable
  token_ Token.Widearrow
  rhs <- expression fixityTable
  return Expr.MatchArm { Expr.rhs, Expr.lhs }

-- | Parses a pattern
pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg ["pattern"]
  $ FixityTable.makeParser FixityTable.Pat pattern' fixityTable
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

  wild = dbg ["pattern", "wild"] $ Pat.Wild <$ token_ Token.Underscore

  lit  = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- M.try to prevent failure from trying to parse infix operator as identifier
  var  = dbg ["pattern", "var"]
    $ M.try (Pat.Var <$> nonfixValueIdentifier fixityTable)

  tup = Pat.Tuple <$> tuple (pattern fixityTable)

  lst = Pat.List <$> list (pattern fixityTable)

-- | @list p@ parses a list, parsing each element with @p@
list :: (Show a) => Parser a -> Parser [a]
list parser = dbg ["list"] $ brackets (parser `sepBy` token_ Token.Comma)

-- | @tuple p@ parses a tuple, parsing each element with @p@
tuple :: (Show a) => Parser a -> Parser [a]
tuple parser = dbg ["tuple"] $ parens (parser `sepBy` token_ Token.Comma)

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
  -- M.try because this could consume a valid bare identifier
  -- | Qualified identifier ending in an alphanumeric identifier
  qualifiedAlphanum :: Parser LongIdent
  qualifiedAlphanum = dbg ["longIdentifier", "qualifiedNonfix"] . M.try $ do
    idents <- alphanumeric `sepBy2` token_ Token.Dot
    let ident = NonEmpty.last idents
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
    qualifiers <- endBy1 (M.try alphanumeric) (token_ Token.Dot)
    ident      <- identifier
    return LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident }

  unqualified :: Parser LongIdent
  unqualified = LongIdent.Ident <$> identifier

-- | Parses a bare identifier
identifier :: Parser Ident
identifier = alphanumeric <|> symbolic

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

-- | Parses a numerical literal
literal :: Parser Lit
literal = dbg ["literal"] $ choice
  [ Lit.Int <$> decimal
  , Lit.Hex <$> hexadecimal
  , Lit.Word <$> word
  , Lit.HexWord <$> hexword
  , Lit.Real <$> real
  -- , Lit.String <$> string
  ]

string :: Parser Text
string = dbg ["string"] $ undefined

-- | Parses an integer literal (in any base)
integer :: Parser Integer
integer = dbg ["integer"] $ choice [decimal, hexadecimal]

-- | Parses a decimal integer literal
decimal :: Parser Integer
decimal = dbg ["decimal"] $ tokenWith
  (\case
    Token.Int i -> Just i
    _           -> Nothing
  )

-- | Parses a hexadecimal integer literal
hexadecimal :: Parser Integer
hexadecimal = dbg ["hexadecimal"] $ tokenWith
  (\case
    Token.Hex i -> Just i
    _           -> Nothing
  )

-- | Parses a word literal
word :: Parser Integer
word = dbg ["word"] $ tokenWith
  (\case
    Token.Word i -> Just i
    _ -> Nothing
  )

-- | Parses a hexadecimal word literal
hexword :: Parser Integer
hexword = dbg ["hexword"] $ tokenWith
  (\case
    Token.HexWord i -> Just i
    _ -> Nothing
  )

-- | Parses a real number
real :: Parser Scientific
real = dbg ["real"] $ tokenWith
  (\case
    Token.Real n -> Just n
    _ -> Nothing
  )

-- | @braces p@ parses @p@ between braces
braces :: (Show a) => Parser a -> Parser a
braces = dbg ["braces"] . between (token_ Token.Lbrace) (token_ Token.Rbrace)

-- | @brackets p@ parses @p@ between brackets
brackets :: (Show a) => Parser a -> Parser a
brackets =
  dbg ["brackets"] . between (token_ Token.Lbracket) (token_ Token.Rbracket)

-- | @parens p@ parses @p@ between parentheses
parens :: (Show a) => Parser a -> Parser a
parens = dbg ["parens"] . between (token_ Token.Lparen) (token_ Token.Rparen)
