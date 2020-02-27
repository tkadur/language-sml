module Parser.Internal where

import qualified Control.Monad                 as Monad
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C

import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Expr                       ( Expr )
import qualified Ast.Expr                      as Expr
import           Ast.Ident.Ident                ( Ident )
import qualified Ast.Ident.Ident               as Ident
import           Ast.Ident.ValueIdent           ( ValueIdent )
import qualified Ast.Ident.ValueIdent          as ValueIdent
import           Ast.Lit                        ( Lit )
import qualified Ast.Lit                       as Lit
import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Reserved       ( Reserved )
import qualified Parser.Internal.Reserved      as Reserved

topLevel :: Parser [Decl]
topLevel = evalStateT decls FixityTable.basisFixityTable
  where decls = M.many declaration

declaration :: StateT FixityTable Parser Decl
declaration = val <|> nonfix <|> infixrDecl <|> infixDecl
 where
  val :: StateT FixityTable Parser Decl
  val = do
    fixityTable <- get
    lift $ do
      reserved Reserved.Val
      lhs <- pattern fixityTable
      reserved Reserved.Equal
      rhs <- expression fixityTable
      return $ Decl.Val { Decl.lhs, Decl.rhs }

  nonfix :: StateT FixityTable Parser Decl
  nonfix = do
    ident <- lift $ do
      reserved Reserved.Nonfix
      identifier
    modify $ FixityTable.removeOperator ident
    return $ Decl.Nonfix { Decl.ident }

  infixrDecl :: StateT FixityTable Parser Decl
  infixrDecl = do
    (precedence, ident) <- lift $ fixityDecl Reserved.Infixr
    modify $ FixityTable.addOperator ident E.InfixR (fromMaybe 0 precedence)
    return $ Decl.Infixr { Decl.precedence, Decl.ident }

  infixDecl :: StateT FixityTable Parser Decl
  infixDecl = do
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
expression fixityTable = FixityTable.makeParser FixityTable.Expr
                                                expression'
                                                fixityTable
 where
  expression' = lit <|> var

  lit         = Expr.Lit <$> literal
  -- M.try to prevent failure from trying to parse infix operator as identifier
  var         = M.try (Expr.Var <$> valueIdentifier operators)

  operators   = FixityTable.operators fixityTable

pattern :: FixityTable -> Parser Pat
pattern fixityTable = FixityTable.makeParser FixityTable.Pat
                                             pattern'
                                             fixityTable
 where
  pattern'  = lit <|> var

  lit       = Pat.Lit <$> literal
  -- M.try to prevent failure from trying to parse infix operator as identifier
  var       = M.try (Pat.Var <$> valueIdentifier operators)

  operators = FixityTable.operators fixityTable

-- | Will only parse identifiers that are not infixed
valueIdentifier :: FixityTable.Operators -> Parser ValueIdent
valueIdentifier operators = ident
 where
  ident = do
    x <- identifier
    if x `HashSet.member` operators
      then fail $ "unexpected infix identifier " ++ show x
      else return $ ValueIdent.fromIdent x

identifier :: Parser Ident
identifier = lexeme $ do
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
  symbolChar = foldl1'
    (<|>)
    (map
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
    )

reserved :: Reserved r => r -> Parser ()
reserved = Monad.void . symbol . Reserved.text

literal :: Parser Lit
literal =
  -- M.try for common prefixes
  M.try (Lit.HexWord <$> prefixed "0wx" hexadecimal)
    <|> M.try (Lit.Hex <$> prefixed "0x" hexadecimal)
    <|> M.try (Lit.DecWord <$> prefixed "0w" decimal)
    <|> (Lit.Dec <$> decimal)
 where
  prefixed :: Text -> Parser Integer -> Parser Integer
  prefixed prefix integer = symbol prefix >> integer
