module Pretty.Comments
  ( Comment
  , Comments
  , split
  , last
  , fromList
  , toList
  , unComment
  )
where

import           Prelude                 hiding ( fromList
                                                , toList
                                                , last
                                                )

import qualified Data.Set                      as Set

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Position                ( Position )
import qualified Lexer

newtype Comments = Comments { unComments :: Set Comment }
  deriving (Show)

newtype Comment = Comment { unComment :: Marked Lexer.Comment }
  deriving (Show)

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

last :: Comments -> Maybe Comment
last = Set.lookupMax . unComments

fromList :: [Marked Lexer.Comment] -> Comments
fromList = Comments . Set.fromList . map Comment

toList :: Comments -> [Comment]
toList = Set.toList . unComments

-- For the purpose of range lookups, we only care about the location of comments
extract :: Comment -> (Position, Position)
extract (Comment Marked.Marked {..}) = (startPosition, endPosition)
