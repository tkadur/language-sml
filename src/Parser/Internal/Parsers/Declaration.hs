module Parser.Internal.Parsers.Declaration where

import           Control.Monad.Combinators      ( choice )
import           Control.Monad.Combinators.NonEmpty
                                                ( some )
import qualified Control.Monad.Combinators.Expr
                                               as E

import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Ident.Ident                ( Ident )
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( bareIdentifier )
import           Parser.Internal.Parsers.Expression
                                                ( expression )
import           Parser.Internal.Parsers.Literal
                                                ( decimal
                                                , hexadecimal
                                                )
import           Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token

-- | Parses a declaration
declaration :: StateT FixityTable Parser Decl
declaration = dbgState ["declaration"]
  $ choice [val, valRec, valAnd, nonfix, infixrDecl, infixDecl]

-- Val declarations
val :: StateT FixityTable Parser Decl
val = dbgState ["declaration", "val"] $ do
  fixityTable <- get
  lift $ do
    token_ Token.Val
    lhs <- pattern fixityTable
    token_ Token.Equal
    rhs <- expression fixityTable
    return Decl.Val { Decl.lhs, Decl.rhs }

valRec :: StateT FixityTable Parser Decl
valRec = dbgState ["declaration", "valRec"] $ do
  fixityTable <- get
  lift $ do
    token_ Token.Val
    token_ Token.Rec
    lhs <- pattern fixityTable
    token_ Token.Equal
    rhs <- expression fixityTable
    return Decl.ValRec { Decl.lhs, Decl.rhs }

valAnd :: StateT FixityTable Parser Decl
valAnd = dbgState ["declaration", "valAnd"] $ do
  fixityTable <- get
  lift $ do
    token_ Token.Val
    token_ Token.Rec
    lhs <- pattern fixityTable
    token_ Token.Equal
    rhs <- expression fixityTable
    return Decl.ValAnd { Decl.lhs, Decl.rhs }

-- Fun declarations

-- Fixity declarations
nonfix :: StateT FixityTable Parser Decl
nonfix = dbgState ["declaration", "nonfix"] $ do
  idents <- lift $ do
    token_ Token.Nonfix
    some bareIdentifier
  modify $ FixityTable.removeOperators idents
  return Decl.Nonfix { Decl.idents }

infixrDecl :: StateT FixityTable Parser Decl
infixrDecl = dbgState ["declaration", "infixrDecl"] $ do
  (precedence, idents) <- lift $ fixityDecl Token.Infixr
  modify $ FixityTable.addOperators idents E.InfixR (fromMaybe 0 precedence)
  return Decl.Infixr { Decl.precedence, Decl.idents }

infixDecl :: StateT FixityTable Parser Decl
infixDecl = dbgState ["declaration", "infixDecl"] $ do
  (precedence, idents) <- lift $ fixityDecl Token.Infix
  modify $ FixityTable.addOperators idents E.InfixL (fromMaybe 0 precedence)
  return Decl.Infix { Decl.precedence, Decl.idents }

fixityDecl :: Token -> Parser (Maybe FixityTable.Precedence, NonEmpty Ident)
fixityDecl keyword = do
  token_ keyword
  precedence <- optional integer
  ident      <- some bareIdentifier
  -- Precedence must either be unspecified or within [0, 9]
  if not $ precedence `elem` (Nothing : map Just [0 .. 9])
    then fail "fixity precedence must be between 0 and 9"
    else return (fromIntegral <$> precedence, ident)
 where
  -- | Parses an integer literal (in any base)
  integer :: Parser Integer
  integer = decimal <|> hexadecimal
