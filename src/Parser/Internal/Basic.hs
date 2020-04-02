module Parser.Internal.Basic
  ( Parser
  , Error
  , Comments
  , DebugLevel(..)
  , dbg
  , dbgState
  , dbgReader
  , nothing
  , never
  , eof
  , token_
  , tokenWith
  , braces
  , brackets
  , parenthesized
  , list
  , tuple
  , xseq
  )
where

import           Control.Monad.Combinators      ( between
                                                , choice
                                                , sepBy
                                                )
import           Control.Monad.Combinators.NonEmpty
                                                ( sepBy1 )
import qualified Control.Monad.Reader          as Reader
import qualified Control.Monad.State.Strict    as State
import           Control.Monad.RWS.Strict       ( RWS )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Set                      as Set
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Debug         as Megaparsec.Debug

import qualified Common.Marked                 as Marked
import           Parser.DebugLevel              ( DebugLevel )
import qualified Parser.DebugLevel             as DebugLevel
import           Parser.Internal.Combinators    ( sepBy2 )
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token
import           Parser.Internal.Stream         ( Stream )

type Comments = [M.SourcePos]

type Error = Void

type UnderlyingMonad = RWS DebugLevel Comments ()

type Parser = M.ParsecT Error Stream UnderlyingMonad

dbg :: (Show a) => DebugLevel.Label -> Parser a -> Parser a
dbg label parser = do
  debug <- ask
  case debug of
    DebugLevel.Off -> parser
    DebugLevel.On  -> dbg_parser
    DebugLevel.ForLabels labels ->
      if any (`isPrefixOf` label) labels then dbg_parser else parser
  where dbg_parser = Megaparsec.Debug.dbg (intercalate "." label) parser

dbgState :: (Show a, Show s)
         => DebugLevel.Label
         -> StateT s Parser a
         -> StateT s Parser a
dbgState = State.mapStateT . dbg

dbgReader :: (Show a)
          => DebugLevel.Label
          -> ReaderT r Parser a
          -> ReaderT r Parser a
dbgReader = Reader.mapReaderT . dbg

-- | Consumes no input and succeeds
nothing :: Parser ()
nothing = return ()

-- | Consumes no input and fails
never :: Parser a
never = empty

eof :: Parser ()
eof = token_ Token.Eof >> M.eof

token_ :: Token -> Parser ()
token_ tok = M.satisfy (\(_, mtok) -> Marked.value mtok == tok) >> nothing

tokenWith :: (Token -> Maybe a) -> Parser a
tokenWith f = M.token (\(_, mtok) -> f $ Marked.value mtok) Set.empty

-- | @braces p@ parses @p@ between braces
braces :: (Show a) => Parser a -> Parser a
braces = dbg ["braces"] . between (token_ Token.Lbrace) (token_ Token.Rbrace)

-- | @brackets p@ parses @p@ between brackets
brackets :: (Show a) => Parser a -> Parser a
brackets =
  dbg ["brackets"] . between (token_ Token.Lbracket) (token_ Token.Rbracket)

-- | @parenthesized p@ parses @p@ between parentheses
parenthesized :: (Show a) => Parser a -> Parser a
parenthesized =
  dbg ["parenthesized"] . between (token_ Token.Lparen) (token_ Token.Rparen)

-- | @list p@ parses a list, parsing each element with @p@
list :: (Show a) => Parser a -> Parser [a]
list parser = dbg ["list"] $ brackets (parser `sepBy` token_ Token.Comma)

-- | @tuple p@ parses a tuple, parsing each element with @p@
tuple :: (Show a) => Parser a -> Parser [a]
tuple parser = dbg ["tuple"]
  $ parenthesized (choice [nonemptyTuple, emptyTuple])
 where
    -- Tuple of at least 2 elements
  nonemptyTuple = NonEmpty.toList <$> parser `sepBy2` token_ Token.Comma

  emptyTuple    = return []

-- | @xseq p@ parses a @pseq@ as given in the definition
xseq :: (Show a) => Parser a -> Parser [a]
xseq parser = dbg ["xseq"] $ choice [sqnce, singleton, emptySeq]
 where
  emptySeq  = return []

  singleton = (: []) <$> parser

  sqnce     = NonEmpty.toList <$> parser `sepBy1` token_ Token.Comma
