module Parser.Internal.Stream
  ( Stream()
  , stream
  )
where

import           Data.List                      ( span )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Relude.Unsafe                 as Unsafe
import qualified Text.Megaparsec               as M

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Position                ( Position )
import qualified Common.Position               as Position
import qualified Lexer.Token

type Input = [(Position, Char)]

type SToken = (Input, Marked Lexer.Token.Token)

type STokens = [SToken]

data Stream = Stream
  { input :: Input
  , tokens :: [Marked Lexer.Token.Token]
  }

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
    (inpt, start) = NonEmpty.head stokens
    (_   , end  ) = NonEmpty.last stokens

  tokensLength :: Proxy Stream -> NonEmpty SToken -> Int
  tokensLength Proxy = length . M.showTokens proxy

  reachOffset :: Int -> M.PosState Stream -> (String, M.PosState Stream)
  reachOffset o posState@M.PosState { M.pstateInput } =
  -- The idea here is that we defer to the behavior of @String@'s @M.Stream@
  -- instance, and then just update the @Stream@ state to match what the
  -- @String@ instance did.
    (string, strPosState { M.pstateInput = strm { tokens = tokens' } })
   where
    (string, strPosState) =
      M.reachOffset o (posState { M.pstateInput = inputToString input })

    -- The suffix of @tokens@ corresponding to @inputSuffix@
    tokens' = case inputSuffix of
      [] -> []
      (position, _) : _ ->
        -- If (for some reason) @inputSuffix@ stops in the middle of a token,
        -- keep that token in @tokens'@
        dropWhile (\token -> Marked.endPosition token < position) tokens

    -- The suffix of the input corresponding to @string@
    inputSuffix = dropUntil (\suffix -> inputToString suffix == string) input

    strm@Stream {..} = pstateInput

-- | @sliceInput startPosition endPosition inpt@ returns the portion
--   of @inpt@ in [@startPosition@, @endPosition@)
sliceInput :: Position -> Position -> Input -> Input
sliceInput startPosition endPosition inp =
  inp |> dropToPosition startPosition |> takeToPosition endPosition
 where
  dropToPosition :: Position -> Input -> Input
  dropToPosition position = dropWhile (\(position', _) -> position' < position)

  takeToPosition :: Position -> Input -> Input
  takeToPosition position = takeWhile (\(position', _) -> position' < position)

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
