module Parser.DebugLevel where

data DebugLevel
  = Off
  | On
  | ForLabels [String]
  deriving (Show)
