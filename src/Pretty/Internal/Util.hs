{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Util where

import           Data.Text.Prettyprint.Doc

import           Common.Positive                ( Positive )
import qualified Common.Positive               as Positive

instance Pretty Positive where
  pretty = pretty . Positive.unPositive
