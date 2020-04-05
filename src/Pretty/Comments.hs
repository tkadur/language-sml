module Pretty.Comments
  ( Comment
  , Comments
  , split
  , fromList
  , toList
  , unComment
  )
where

import           Prelude                 hiding ( fromList
                                                , toList
                                                )

import qualified Data.Set                      as Set

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Position                ( Position )
import qualified Lexer

newtype Comments = Comments { unComments :: Set Comment }

newtype Comment = Comment (Marked Lexer.Comment)

instance Eq Comment where
  (==) = (==) `on` extract

instance Ord Comment where
  compare = compare `on` extract

split :: Marked a -> Comments -> (Comments, Comments)
split hi comments =
  comments
    |> unComments
    -- Remove comments after hi
    |> Set.split dummyhi
    |> bimap Comments Comments
  where dummyhi = Comment (hi { Marked.value = "" })

fromList :: [Marked Lexer.Comment] -> Comments
fromList = Comments . Set.fromList . map Comment

toList :: Comments -> [Comment]
toList = Set.toList . unComments

unComment :: Comment -> Lexer.Comment
unComment (Comment m) = Marked.value m

-- For the purpose of range lookups, we only care about the location of comments
extract :: Comment -> (Position, Position)
extract (Comment Marked.Marked {..}) = (startPosition, endPosition)
