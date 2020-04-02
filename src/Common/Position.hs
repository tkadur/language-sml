module Common.Position
  ( Position(..)
  )
where

import           Common.Positive                ( Positive )

data Position = Position
  { file :: FilePath
  , line :: Positive
  , col :: Positive
  }
  deriving (Eq, Ord, Show)
