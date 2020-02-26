module Parser.Internal.Basic
    ( Parser
    , E
    , S
    , M
    , Comments
    , commaSeparated
    , lexeme
    , nothing
    , decimal
    , hexadecimal
    , parens
    , stringLiteral
    , symbol
    )
where

import           Control.Monad.Writer.Strict    ( Writer )
import qualified Control.Monad                 as Monad
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L

type Comments = [M.SourcePos]

-- Error
type E = Void
-- Stream
type S = Text
-- Underlying monad
type M = Writer Comments

type Parser = M.ParsecT E S M

nothing :: Parser ()
nothing = Monad.void $ symbol ""

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`M.sepBy` (lexeme . C.char) ',')

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

stringLiteral :: Parser Text
stringLiteral =
    (lexeme . fmap toText) $ C.char '"' >> M.manyTill L.charLiteral (C.char '"')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty blockComment
    where blockComment = L.skipBlockComment "(*" "*)"
