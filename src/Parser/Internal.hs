module Parser.Internal where

import qualified Control.Monad                 as Monad
import           Control.Monad.Combinators
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C

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

topLevel :: Parser [Decl]
topLevel = dbg "topLevel" $ evalStateT decls FixityTable.basisFixityTable
  where decls = M.many declaration

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

expression :: FixityTable -> Parser Expr
expression fixityTable = dbg "expression"
  $ FixityTable.makeParser FixityTable.Expr expression' fixityTable
 where
  expression' = choice [lit, var]

  lit         = Expr.Lit <$> literal
  -- M.try to prevent failure from trying to parse infix operator as identifier
  var         = M.try (Expr.Var <$> valueIdentifier operators NonfixOnly)

  operators   = FixityTable.operators fixityTable

pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg "pattern"
  $ FixityTable.makeParser FixityTable.Pat pattern' fixityTable
 where
  pattern'  = choice [lit, var]

  lit       = Pat.Lit <$> literal
  -- M.try to prevent failure from trying to parse infix operator as identifier
  var       = M.try (Pat.Var <$> valueIdentifier operators NonfixOnly)

  operators = FixityTable.operators fixityTable

valueIdentifier :: FixityTable.Operators -> AllowedFixity -> Parser ValueIdent
valueIdentifier operators allowedFixity = dbg "valueIdentifier"
  $ choice
    -- M.try since will try to parse "op" as an identifier
           [M.try longIdent, op]
 where
  op = do
    reserved Reserved.Op
    ValueIdent.Op <$> valueIdentifier operators AnyFixity

  longIdent = ValueIdent.LongIdent <$> longIdentifier operators allowedFixity

longIdentifier :: FixityTable.Operators -> AllowedFixity -> Parser LongIdent
longIdentifier operators allowedFixity = dbg "longIdentifier"
  $ choice [qualifiedNonfix, LongIdent.Ident <$> ident allowedFixity]
 where
  -- M.try because this could consume a valid bare identifier
  qualifiedNonfix = M.try $ do
    idents <- sepBy2 (ident NonfixOnly) (symbol ".")
    let x          = NonEmpty.last idents
    let qualifiers = NonEmpty.take (NonEmpty.length idents - 1) idents
    return $ LongIdent.Qualified { LongIdent.qualifiers, LongIdent.ident = x }

  ident fixityAllowed = do
    x <- identifier
    case fixityAllowed of
      AnyFixity  -> return x
      NonfixOnly -> if x `HashSet.member` operators
        then fail $ "unexpected infix identifier " ++ show x
        else return x

identifier :: Parser Ident
identifier = dbg "identifier" . lexeme $ do
  ident <- toText <$> (alphanumeric <|> symbolic)
  -- Make sure the identifier isn't a reserved keyword (other than =)
  if ident `elem` Reserved.reservedTokens && ident /= "="
    then fail $ "keyword " ++ show ident ++ " cannot be an identifier"
    else return $ Ident.Ident ident
 where
  -- Alphanumeric identifiers
  alphanumeric =
    (:)
      -- The first character must be a letter or underscore
      <$> (C.letterChar <|> prime)
      -- The remaining characters can be alphanumeric or underscores
      <*> M.many (C.alphaNumChar <|> underscore <|> prime)
  underscore = C.char '_'
  prime      = C.char '\''

  -- Symbolic identifiers
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

reserved :: Reserved r => r -> Parser ()
reserved = dbg "reserved" . Monad.void . symbol . Reserved.text

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
