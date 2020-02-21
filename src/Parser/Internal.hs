module Parser.Internal where

import qualified Control.Monad                 as Monad
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C

import qualified Ast
import qualified Ast.Decl
import qualified Ast.Expr
import qualified Ast.Ident
import qualified Ast.Lit
import qualified Ast.Pat
import           Parser.Basic
import           Parser.Reserved                ( Reserved )
import qualified Parser.Reserved               as Reserved

declaration :: Parser Ast.Decl
declaration = val
 where
  val = do
    reserved Reserved.Val
    pat <- pattern
    reserved Reserved.Equal
    expr <- expression
    return $ Ast.Decl.Val pat expr

expression :: Parser Ast.Expr
expression = lit <|> var
 where
  lit = Ast.Expr.Lit <$> literal
  var = Ast.Expr.Var <$> identifier

pattern :: Parser Ast.Pat
pattern = lit <|> var
 where
  lit = Ast.Pat.Lit <$> literal
  var = Ast.Pat.Var <$> identifier

identifier :: Parser Ast.Ident
identifier = lexeme $ do
  ident <- toText <$> (alphanum <|> symbol)
  -- Make sure the identifier isn't a reserved keyword
  if ident `elem` Reserved.reservedTokens
    then fail $ "Keyword " ++ show ident ++ " cannot be an identifier"
    else return $ Ast.Ident.Ident ident
 where
  underscore = C.char '_'
  -- Alphanumeric identifiers
  alphanum =
    (:)
      -- The first character must be a letter or underscore
      <$> (C.letterChar <|> underscore)
      -- The remaining characters can be alphanumeric or underscores
      <*> M.many (C.alphaNumChar <|> underscore)

  -- Symbolic identifiers
  symbol = some (C.symbolChar <|> underscore)

reserved :: Reserved a => a -> Parser ()
reserved = Monad.void . symbol . Reserved.text

literal :: Parser Ast.Lit
literal = (Ast.Lit.Int <$> intLiteral) <|> (Ast.Lit.String <$> stringLiteral)
