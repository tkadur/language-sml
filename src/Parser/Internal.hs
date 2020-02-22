module Parser.Internal where

import qualified Control.Monad                 as Monad
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Data.HashSet                  as HashSet
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C

import           Ast                            ( Decl
                                                , Expr
                                                , Ident
                                                , Lit
                                                , Pat
                                                )
import qualified Ast.Decl                      as Decl
import qualified Ast.Expr                      as Expr
import qualified Ast.Ident                     as Ident
import qualified Ast.Lit                       as Lit
import qualified Ast.Pat                       as Pat
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Reserved       ( Reserved )
import qualified Parser.Internal.Reserved      as Reserved

type Operators = HashSet Ident

topLevel :: Parser [Decl]
topLevel = evalStateT decls FixityTable.basisFixityTable
  where decls = M.many declaration

declaration :: StateT FixityTable Parser Decl
declaration = val <|> infixrDecl <|> infixDecl
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

  infixrDecl :: StateT FixityTable Parser Decl
  infixrDecl = do
    fixityTable         <- get
    (precedence, ident) <- lift $ fixityDecl Reserved.Infixr
    put (FixityTable.addOperator ident E.InfixR precedence fixityTable)
    return $ Decl.Infixr { Decl.precedence, Decl.ident }

  infixDecl :: StateT FixityTable Parser Decl
  infixDecl = do
    fixityTable         <- get
    (precedence, ident) <- lift $ fixityDecl Reserved.Infix
    put (FixityTable.addOperator ident E.InfixL precedence fixityTable)
    return $ Decl.Infix { Decl.precedence, Decl.ident }

  fixityDecl :: (Reserved r) => r -> Parser (FixityTable.Precedence, Ident)
  fixityDecl keyword = do
    -- M.try to avoid "infix" and "infixr" clashing
    M.try $ reserved keyword
    precedence <- fromMaybe 0 <$> M.optional intLiteral
    ident      <- rawIdentifier
    if not $ precedence `elem` [0 .. 9]
      then fail "fixity precedence must be between 0 and 9"
      else return $ (precedence, ident)


expression :: FixityTable -> Parser Expr
expression fixityTable = FixityTable.makeExprParser expr' fixityTable
 where
  expr'     = lit <|> var

  lit       = Expr.Lit <$> literal
  -- M.try to prevent failure from trying to parse infix operator as identifier
  var       = Expr.Var <$> M.try (nonfixIdentifier operators)

  operators = FixityTable.operators fixityTable

pattern :: FixityTable -> Parser Pat
pattern fixityTable = lit <|> var
 where
  lit       = Pat.Lit <$> literal
  var       = Pat.Var <$> nonfixIdentifier operators
  --
  operators = FixityTable.operators fixityTable

-- Parses an identifier which must be nonfixed
nonfixIdentifier :: Operators -> Parser Ident
nonfixIdentifier operators = do
  ident <- rawIdentifier
  if ident `HashSet.member` operators
    then
      fail $ "prefix identifier " ++ show ident ++ " appears in nonfix context"
    else return ident

-- Parses anything which could be an identifier
rawIdentifier :: Parser Ident
rawIdentifier = lexeme $ do
  ident <- toText <$> (alphanumeric <|> symbolic)
  -- Make sure the identifier isn't a reserved keyword
  if ident `elem` Reserved.reservedTokens
    then fail $ "keyword " ++ show ident ++ " cannot be an identifier"
    else return $ Ident.Ident ident
 where
  underscore = C.char '_'
  -- Alphanumeric identifiers
  alphanumeric =
    (:)
          -- The first character must be a letter or underscore
      <$> (C.letterChar <|> underscore)
          -- The remaining characters can be alphanumeric or underscores
      <*> M.many (C.alphaNumChar <|> underscore)

  -- Symbolic identifiers
  symbolic = some (C.symbolChar <|> underscore)

reserved :: Reserved a => a -> Parser ()
reserved = Monad.void . symbol . Reserved.text

literal :: Parser Lit
literal = (Lit.Int <$> intLiteral) <|> (Lit.String <$> stringLiteral)
