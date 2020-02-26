module Parser.Internal.Basic
    ( Parser
    , E
    , S
    , M
    , Comments
    , lexeme
    , nothing
    , decimal
    , hexadecimal
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

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty blockComment
    where blockComment = L.skipBlockComment "(*" "*)"
