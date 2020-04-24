module Language.Sml.Parser.Internal.Stream
  ( Stream()
  , stream
  , tokens
  )
where

import           Data.List                      ( span )
import qualified Relude.Unsafe                 as Unsafe
import qualified Text.Megaparsec               as M

import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Common.Marked    as Marked
import           Language.Sml.Common.Position   ( Position )
import qualified Language.Sml.Common.Position  as Position
import           Language.Sml.Common.Positive   ( Positive )
import qualified Language.Sml.Common.Positive  as Positive
import qualified Language.Sml.Lexer.Token      as Lexer.Token

type Input = [(Position, Char)]

type SToken = (Input, Marked Lexer.Token.Token)

type STokens = [SToken]

data Stream = Stream
  { input :: Input
  , tokens :: [Marked Lexer.Token.Token]
  }
  deriving (Show)

instance M.Stream Stream where
  type Token Stream = SToken
  type Tokens Stream = STokens

  tokensToChunk :: Proxy Stream -> [SToken] -> STokens
  tokensToChunk Proxy = id

  chunkToTokens :: Proxy Stream -> STokens -> [SToken]
  chunkToTokens Proxy = id

  chunkLength :: Proxy Stream -> STokens -> Int
  chunkLength Proxy = length

  chunkEmpty :: Proxy Stream -> STokens -> Bool
  chunkEmpty Proxy = null

  take1_ :: Stream -> Maybe (SToken, Stream)
  take1_ strm = unwrapTokens <$> M.takeN_ 1 strm
    where unwrapTokens (tokens, strm') = (Unsafe.head tokens, strm')

  takeN_ :: Int -> Stream -> Maybe (STokens, Stream)
  takeN_ n strm@Stream {..}
    | n <= 0      = Just ([], strm)
    | null tokens = Nothing
    | otherwise   = Just $ take_ strm (splitAt n $ strmToStokens strm)

  takeWhile_ :: (SToken -> Bool) -> Stream -> (STokens, Stream)
  takeWhile_ f strm = take_ strm (span f $ strmToStokens strm)

  showTokens :: Proxy Stream -> NonEmpty SToken -> String
  showTokens Proxy stokens = inputToString
    $ sliceInput (Marked.startPosition start) (Marked.endPosition end) inpt
   where
    (inpt, start) = head stokens
    (_   , end  ) = last stokens

  tokensLength :: Proxy Stream -> NonEmpty SToken -> Int
  tokensLength Proxy = length . M.showTokens proxy

  reachOffset :: Int -> M.PosState Stream -> (String, M.PosState Stream)
  reachOffset offset M.PosState {..} = (offendingLine, posState')
   where
    offendingLine =
      let res = inputToString $ filter
            (\(Position.Position { line }, c) ->
              -- Only keep the desired line and don't include newline at end
              c /= '\n' && positiveToPos line == M.sourceLine pstateSourcePos'
            )
            input
      in  if res == ""
            -- Handle empty line
            then "<empty line>"
            -- Replace tabs with spaces
            else concatMap
              (\case
                '\t' -> replicate (M.unPos pstateTabWidth) ' '
                c    -> [c]
              )
              res

    posState' = M.PosState { pstateInput      = pstateInput'
                           , pstateOffset     = offset
                           , pstateSourcePos  = pstateSourcePos'
                           , pstateTabWidth
                           , pstateLinePrefix
                           }

    tokens'          = drop offset tokens
    pstateInput'     = Stream { input, tokens = tokens' }
    pstateSourcePos' = case (tokens !!? (length tokens - 1), tokens' !!? 0) of
      (_, Just token) -> positionToSourcePos $ Marked.startPosition token
      (Just token, Nothing) -> positionToSourcePos $ Marked.endPosition token
      (Nothing, Nothing) -> pstateSourcePos

    Stream { input, tokens } = pstateInput

    positiveToPos :: Positive -> M.Pos
    positiveToPos = M.mkPos . fromInteger . Positive.unPositive

    positionToSourcePos :: Position -> M.SourcePos
    positionToSourcePos Position.Position {..} = M.SourcePos
      { M.sourceName   = file
      , M.sourceLine   = positiveToPos line
      , M.sourceColumn = positiveToPos col
      }

-- | @sliceInput startPosition endPosition inpt@ returns the portion
--   of @inpt@ in [@startPosition@, @endPosition@)
sliceInput :: Position -> Position -> Input -> Input
sliceInput startPosition endPosition inpt =
  inpt |> dropWhile (\(position, _) -> position < startPosition) |> takeWhile
    (\(position, _) -> position < endPosition)

-- Factors out common functionality for takeN_ and takeWhile_
take_ :: Stream -> (STokens, STokens) -> (STokens, Stream)
take_ strm (res, stokens) = (res, strm { tokens = map snd stokens })

stream :: FilePath                   -- ^ Source file name
       -> Text                       -- ^ Source file contents
       -> [Marked Lexer.Token.Token] -- ^ Lexed token stream
       -> Stream
stream file rawInput tokens = Stream { input, tokens }
 where
  input = mark $ toString rawInput

  -- | Marks every character with its position
  mark :: String -> Input
  mark = foldingMap
    (\(line, col) c ->
      -- Update position based on current character
      ( case c of
        '\n' -> (line + 1, 0)
        _    -> (line, col + 1)
      -- Mark each character with current position
      , (Position.Position { Position.file, Position.line, Position.col }, c)
      )
    )
    -- Initial position is 1:1
    (1, 1)

strmToStokens :: Stream -> STokens
strmToStokens Stream {..} = map (input, ) tokens

inputToString :: Input -> String
inputToString = map snd

-- | Specialized version of @Proxy@ to aid type inference and avoid
--   the need for explicit type application.
proxy :: Proxy Stream
proxy = Proxy
