module Language.Sml.Common.Position
  ( Position(..)
  )
where

import           Language.Sml.Common.Positive   ( Positive )

data Position = Position
  { file :: FilePath
  , line :: Positive
  , col :: Positive
  }
  deriving (Eq, Ord, Show)
