module Common.Position
  ( Position(..)
  )
where

data Position = Position
  { file :: FilePath
  , line :: Natural
  , col :: Natural
  }
  deriving (Eq, Ord, Show)
