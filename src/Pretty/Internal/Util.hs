{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Util where

import           Data.Text.Prettyprint.Doc

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Positive                ( Positive )
import qualified Common.Positive               as Positive

instance Pretty Positive where
  pretty = pretty . (Positive.unPositive @Integer)

instance (Pretty a) => Pretty (Marked a) where
  pretty = pretty . Marked.value

hpcat :: (Pretty a) => [a] -> Doc ann
hpcat = hcat . map pretty
