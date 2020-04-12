module Pretty
  ( Pretty(..)
  , Config(..)
  , prettyPrint
  )
where

import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as Render


import           Pretty.Comments                ( Comments )
import           Pretty.Internal.Basic          ( Pretty(..)
                                                , evalDocState
                                                )

data Config =
  Config
  { lineLength :: Int
  , indentWidth :: Int
  }

prettyPrint :: (Pretty a) => Config -> Comments -> a -> Text
prettyPrint Config { lineLength, indentWidth } comments x =
  Render.renderStrict $ Doc.layoutPretty
    (Doc.LayoutOptions { Doc.layoutPageWidth = Doc.AvailablePerLine lineLength 1
                       }
    )
    (evalDocState indentWidth comments $ pretty x)
