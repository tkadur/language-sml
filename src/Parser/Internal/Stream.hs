module Parser.Internal.Stream
  ( Stream()
  )
where

import           Data.List                      ( span )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Relude.Unsafe                 as Unsafe
import qualified Text.Megaparsec               as M

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Position                ( Position )
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
  take1_ stream = unwrapTokens <$> M.takeN_ 1 stream
    where unwrapTokens (tokens, stream') = (Unsafe.head tokens, stream')

  takeN_ :: Int -> Stream -> Maybe (STokens, Stream)
  takeN_ n stream@Stream {..}
    | n <= 0      = Just ([], stream)
    | null tokens = Nothing
    | otherwise   = Just $ take_ stream (splitAt n $ streamToStokens stream)

  takeWhile_ :: (SToken -> Bool) -> Stream -> (STokens, Stream)
  takeWhile_ f stream = take_ stream (span f $ streamToStokens stream)

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
    (string, strPosState { M.pstateInput = stream { tokens = tokens' } })
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

    stream@Stream {..} = pstateInput

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
take_ stream (res, stokens) = (res, stream { tokens = map snd stokens })

streamToStokens :: Stream -> STokens
streamToStokens Stream {..} = map (input, ) tokens

inputToString :: Input -> String
inputToString = map snd

-- | Specialized version of @Proxy@ to aid type inference and avoid
--   the need for explicit type application.
proxy :: Proxy Stream
proxy = Proxy
