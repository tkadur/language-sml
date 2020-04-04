module Parser.Internal.Basic
  ( MonadParser
  , Parser
  , Error
  , Comments
  , DebugLevel(..)
  , dbg
  , liftParser
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
  , marked
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

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import qualified Common.Position               as Position
import qualified Common.Positive               as Positive
import           Parser.DebugLevel              ( DebugLevel )
import qualified Parser.DebugLevel             as DebugLevel
import           Parser.Internal.Combinators    ( sepBy2 )
import           Parser.Internal.Token          ( Token )
import qualified Parser.Internal.Token         as Token
import           Parser.Internal.Stream         ( Stream )
import qualified Parser.Internal.Stream        as Stream

type Comments = [M.SourcePos]

type Error = Void

type UnderlyingMonad = RWS DebugLevel Comments ()

type Parser = M.ParsecT Error Stream UnderlyingMonad

class (MonadFail parser, M.MonadParsec Error Stream parser) => MonadParser parser where
  dbg :: (Show a) => DebugLevel.Label -> parser a -> parser a

  liftParser :: Parser a -> parser a

instance MonadParser Parser where
  dbg label parser = do
    debug <- ask
    case debug of
      DebugLevel.Off -> parser
      DebugLevel.On  -> dbg_parser
      DebugLevel.ForLabels labels ->
        if any (`isPrefixOf` label) labels then dbg_parser else parser
    where dbg_parser = Megaparsec.Debug.dbg (intercalate "." label) parser

  liftParser = id

instance (MonadParser parser, Show s) => MonadParser (StateT s parser) where
  dbg        = State.mapStateT . dbg

  liftParser = lift . liftParser

instance (MonadParser parser) => MonadParser (ReaderT r parser) where
  dbg        = Reader.mapReaderT . dbg

  liftParser = lift . liftParser

-- | Consumes no input and succeeds
nothing :: (MonadParser parser) => parser ()
nothing = return ()

-- | Consumes no input and fails
never :: (MonadParser parser) => parser a
never = empty

eof :: (MonadParser parser) => parser ()
eof = token_ Token.Eof >> M.eof

token_ :: (MonadParser parser) => Token -> parser ()
token_ tok = M.satisfy (\(_, mtok) -> Marked.value mtok == tok) >> nothing

tokenWith :: (MonadParser parser) => (Token -> Maybe a) -> parser a
tokenWith f = M.token (\(_, mtok) -> f $ Marked.value mtok) Set.empty

-- | @braces p@ parses @p@ between braces
braces :: (MonadParser parser, Show a) => parser a -> parser a
braces = dbg ["braces"] . between (token_ Token.Lbrace) (token_ Token.Rbrace)

-- | @brackets p@ parses @p@ between brackets
brackets :: (MonadParser parser, Show a) => parser a -> parser a
brackets =
  dbg ["brackets"] . between (token_ Token.Lbracket) (token_ Token.Rbracket)

-- | @parenthesized p@ parses @p@ between parentheses
parenthesized :: (MonadParser parser, Show a) => parser a -> parser a
parenthesized =
  dbg ["parenthesized"] . between (token_ Token.Lparen) (token_ Token.Rparen)

-- | @list p@ parses a list, parsing each element with @p@
list :: (MonadParser parser, Show a) => parser a -> parser [a]
list parser = dbg ["list"] $ brackets (parser `sepBy` token_ Token.Comma)

-- | @tuple p@ parses a tuple, parsing each element with @p@
tuple :: (MonadParser parser, Show a) => parser a -> parser [a]
tuple parser = dbg ["tuple"]
  $ parenthesized (choice [nonemptyTuple, emptyTuple])
 where
    -- Tuple of at least 2 elements
  nonemptyTuple = NonEmpty.toList <$> parser `sepBy2` token_ Token.Comma

  emptyTuple    = return []

-- | @xseq p@ parses a @pseq@ as given in the definition
xseq :: (MonadParser parser, Show a) => parser a -> parser [a]
xseq parser = dbg ["xseq"] $ choice [sqnce, singleton, emptySeq]
 where
  emptySeq  = dbg ["xseq", "emptySeq"] $ return []

  singleton = dbg ["xseq", "singleton"] $ (: []) <$> parser

  sqnce     = dbg ["xseq", "sequence"] $ NonEmpty.toList <$> parenthesized
    (parser `sepBy1` token_ Token.Comma)


marked :: (MonadParser parser) => parser a -> parser (Marked a)
marked parser = do
  (parsedTokens, value) <- M.match parser
  remainingTokens       <- Stream.tokens <$> M.getInput

  case (nonEmpty parsedTokens, nonEmpty remainingTokens) of
    (Just tokens, _) ->
      let ((_, start), (_, end)) = (head tokens, last tokens)
      in  return $ Marked.merge start end value
    -- If nothing was consumed, take our best guess
    (Nothing, Just tokens) ->
      let Marked.Marked { Marked.startPosition } = head tokens
      in  return $ Marked.Marked { Marked.value
                                 , Marked.startPosition
                                 , Marked.endPosition   = startPosition
                                 }
    -- If nothing was consumed and no input is left, last rsort
    (Nothing, Nothing) -> do
      M.SourcePos {..} <- M.getSourcePos

      let position = Position.Position
            { Position.file = sourceName
            , Position.line = Positive.positive (M.unPos sourceLine)
            , Position.col  = Positive.positive (M.unPos sourceColumn)
            }

      return $ Marked.Marked { Marked.value
                             , Marked.startPosition = position
                             , Marked.endPosition   = position
                             }

