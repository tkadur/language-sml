module Common.Marked
  ( Marked(..)
  )
where

import           Common.Position                ( Position )

data Marked a = Marked
  { value :: a
  , startPosition :: Position
  , endPosition :: Position
  , len :: Int
  }
  deriving (Eq, Ord, Show)
