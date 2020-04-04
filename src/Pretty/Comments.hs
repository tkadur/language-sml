module Pretty.Comments
  ( Comments
  , between
  , fromList
  , toList
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

newtype Comment = Comment { unComment :: Marked Lexer.Comment }

instance Eq Comment where
  (==) = (==) `on` extract

instance Ord Comment where
  compare = compare `on` extract

between :: Marked a -> Marked b -> Comments -> Comments
between lo hi comments =
  comments
    |> unComments
    -- Remove comments before lo
    |> (snd . Set.split dummylo)
    -- Remove comments after hi
    |> (fst . Set.split dummyhi)
    |> Comments
 where
  dummylo = Comment (lo { Marked.value = "" })
  dummyhi = Comment (hi { Marked.value = "" })

fromList :: [Marked Lexer.Comment] -> Comments
fromList = Comments . Set.fromList . map Comment

toList :: Comments -> [Marked Lexer.Comment]
toList = map unComment . Set.toList . unComments

-- For the purpose of range lookups, we only care about the location of comments
extract :: Comment -> (Position, Position)
extract (Comment Marked.Marked {..}) = (startPosition, endPosition)
