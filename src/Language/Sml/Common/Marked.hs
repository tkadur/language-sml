module Language.Sml.Common.Marked
  ( Marked(..)
  , merge
  , replace
  )
where

import           Language.Sml.Common.Position   ( Position )

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

-- | @merge m1 m2 x@ has the @startPosition@ of @m1@, the
--   @endPosition@ of @m2@, and @x@ as the value.
merge :: Marked a -> Marked b -> c -> Marked c
merge Marked { startPosition } Marked { endPosition } value =
  Marked { value, startPosition, endPosition }

replace :: Marked a -> b -> Marked b
replace marked x = marked { value = x }
