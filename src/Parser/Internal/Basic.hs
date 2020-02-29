module Parser.Internal.Basic
  ( Parser
  , E
  , S
  , M
  , Comments
  , DebugLevel(..)
  , lexeme
  , nothing
  , decimal
  , hexadecimal
  , symbol
  , dbg
  , dbgState
  , dbgReader
  )
where

import qualified Control.Monad.Reader          as Reader
import qualified Control.Monad.State.Strict    as State
import           Control.Monad.RWS.Strict       ( RWS )
import qualified Debug.Trace
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Text.Megaparsec.Debug         as Megaparsec.Debug

type Comments = [M.SourcePos]

data DebugLevel
  = Off
  | On
  | ForLabels [String]

-- Error
type E = Void

-- Stream
type S = Text

-- Underlying monad
type M = RWS DebugLevel Comments ()

type Parser = M.ParsecT E S M

dbg :: (Show a) => String -> Parser a -> Parser a
dbg label parser = do
  debug <- ask
  case debug of
    Off -> parser
    On  -> dbg_parser
    ForLabels labels -> if label `elem` labels then dbg_parser else parser
  where dbg_parser = Megaparsec.Debug.dbg label parser

dbgState :: (Show a, Show s) => String -> StateT s Parser a -> StateT s Parser a
dbgState = State.mapStateT . dbg

dbgReader :: (Show a) => String -> ReaderT r Parser a -> ReaderT r Parser a
dbgReader = Reader.mapReaderT . dbg

nothing :: Parser ()
nothing = return ()

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
