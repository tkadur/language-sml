module Parser.Internal.Parsers.Declaration where

import           Control.Monad.Combinators      ( choice
                                                , many
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( some )
import qualified Control.Monad.Combinators.Expr
                                               as E
import qualified Text.Megaparsec               as M

import           Ast.Decl                       ( Decl )
import qualified Ast.Decl                      as Decl
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( valueIdentifier
                                                , typeVariable
                                                )
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
declaration =
  dbgState ["declaration"] $ choice [val, nonfix, infixrDecl, infixDecl]

-- Val declarations

val :: StateT FixityTable Parser Decl
val = do
  lift $ token_ Token.Val

  tyvars    <- lift $ xseq typeVariable

  firstBind <- valBind nothing
  andBinds  <- many $ valBind (token_ Token.And)

  return Decl.Val { Decl.tyvars, Decl.valbinds = firstBind :| andBinds }

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

-- val :: StateT FixityTable Parser Decl
-- val = dbgState ["declaration", "val"] $ do
--   fixityTable <- get
--   lift $ do
--     token_ Token.Val
--     let tyvars = []
--     lhs <- pattern fixityTable
--     token_ Token.Equal
--     rhs <- expression fixityTable
--     return Decl.Val { Decl.tyvars, Decl.lhs, Decl.rhs }

-- valRec :: StateT FixityTable Parser Decl
-- valRec = dbgState ["declaration", "valRec"] $ do
--   fixityTable <- get
--   lift $ do
--     token_ Token.Val
--     let tyvars = []
--     token_ Token.Rec
--     lhs <- pattern fixityTable
--     token_ Token.Equal
--     rhs <- expression fixityTable
--     return Decl.ValRec { Decl.tyvars, Decl.lhs, Decl.rhs }

-- valAnd :: StateT FixityTable Parser Decl
-- valAnd = dbgState ["declaration", "valAnd"] $ do
--   fixityTable <- get
--   lift $ do
--     token_ Token.Val
--     let tyvars = []
--     token_ Token.Rec
--     lhs <- pattern fixityTable
--     token_ Token.Equal
--     rhs <- expression fixityTable
--     return Decl.ValAnd { Decl.tyvars, Decl.lhs, Decl.rhs }

-- Fun declarations

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
  modify $ FixityTable.addOperators idents E.InfixR (fromMaybe 0 precedence)
  return Decl.Infixr { Decl.precedence, Decl.idents }

infixDecl :: StateT FixityTable Parser Decl
infixDecl = dbgState ["declaration", "infixDecl"] $ do
  (precedence, idents) <- lift $ fixityDecl Token.Infix
  modify $ FixityTable.addOperators idents E.InfixL (fromMaybe 0 precedence)
  return Decl.Infix { Decl.precedence, Decl.idents }

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
