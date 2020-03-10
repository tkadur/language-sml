module Common.Marked
  ( Marked(..)
  )
where

import           Common.Position                ( Position )

data Marked a = Marked
  { value :: a
  , position :: Position
  }
  deriving (Show)
