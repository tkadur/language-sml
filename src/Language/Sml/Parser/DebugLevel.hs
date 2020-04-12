module Language.Sml.Parser.DebugLevel where

data DebugLevel
  = Off
  | On
  | ForLabels [[String]]
  deriving (Show)

type Label = [String]
