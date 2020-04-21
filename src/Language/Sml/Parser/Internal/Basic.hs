module Language.Sml.Parser.Internal.Basic
  ( MonadParser
  , Parser
  , Error
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
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Set                      as Set
import qualified Data.Vector                   as Vector
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Debug         as Megaparsec.Debug

import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Common.Marked    as Marked
import qualified Language.Sml.Common.Position  as Position
import qualified Language.Sml.Common.Positive  as Positive
import           Language.Sml.Parser.DebugLevel ( DebugLevel )
import qualified Language.Sml.Parser.DebugLevel
                                               as DebugLevel
import           Language.Sml.Parser.Internal.Combinators
                                                ( sepBy2 )
import           Language.Sml.Parser.Internal.Token
                                                ( Token )
import qualified Language.Sml.Parser.Internal.Token
                                               as Token
import           Language.Sml.Parser.Internal.Stream
                                                ( Stream )
import qualified Language.Sml.Parser.Internal.Stream
                                               as Stream

type Error = Void

type UnderlyingMonad = Reader DebugLevel

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
-- @try@ to avoid consuming parens from the start of a pattern instead of
-- from a xseq.
-- TODO(tkadur) This is a huge hack and only works cause @xseq@ is
-- only ever used for tyvarseqs
xseq parser = dbg ["xseq"] $ choice [M.try sqnce, singleton, emptySeq]
 where
  emptySeq  = dbg ["xseq", "emptySeq"] $ return []

  singleton = dbg ["xseq", "singleton"] $ (: []) <$> parser

  sqnce     = dbg ["xseq", "sequence"] $ NonEmpty.toList <$> parenthesized
    (parser `sepBy1` token_ Token.Comma)


marked :: (MonadParser parser) => parser a -> parser (Marked a)
marked parser = do
  (parsedTokens, value) <- M.match parser
  remainingTokens       <- Stream.tokens <$> M.getInput

  case (Vector.null parsedTokens, Vector.null remainingTokens) of
    (False, _) ->
      let ((_, start), (_, end)) =
              (Vector.head parsedTokens, Vector.last parsedTokens)
      in  return $ Marked.merge start end value
    -- If nothing was consumed, take our best guess
    (True, False) ->
      let Marked.Marked { Marked.startPosition } = Vector.head remainingTokens
      in  return $ Marked.Marked { Marked.value
                                 , Marked.startPosition
                                 , Marked.endPosition   = startPosition
                                 }
    -- If nothing was consumed and no input is left, last rsort
    (True, True) -> do
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

