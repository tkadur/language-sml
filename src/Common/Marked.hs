module Common.Marked
  ( Marked(..)
  )
where

import           Common.Position                ( Position )

data Marked a = Marked
  { value :: a
  , startPosition :: Position
  , endPosition :: Position
  }
  deriving (Functor, Show)

instance (Eq a) => Eq (Marked a) where
  (==) = (==) `on` value

instance (Ord a) => Ord (Marked a) where
  compare = compare `on` value
