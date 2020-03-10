module Common.Position
  ( Position(..)
  )
where

data Position = Position
  { file :: FilePath
  , line :: Int
  , col :: Int
  }
  deriving (Show)
