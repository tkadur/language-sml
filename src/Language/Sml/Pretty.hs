module Language.Sml.Pretty
  ( module Toplevel
  , Pretty(..)
  , Config(..)
  , prettyPrint
  )
where

import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as Render


import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Lexer            as Lexer
import qualified Language.Sml.Pretty.Comments  as Comments
import           Language.Sml.Pretty.Internal.Basic
                                                ( Pretty(..)
                                                , evalDocState
                                                )
import qualified Language.Sml.Pretty.Internal.Printers.Toplevel
                                               as Toplevel
                                                ( )

data Config =
  Config
  { lineLength :: Int
  , indentWidth :: Int
  }

prettyPrint :: (Pretty a) => Config -> [Marked Lexer.Comment] -> a -> Text
prettyPrint Config { lineLength, indentWidth } comments x =
  Render.renderStrict $ Doc.layoutPretty
    (Doc.LayoutOptions { Doc.layoutPageWidth = Doc.AvailablePerLine lineLength 1
                       }
    )
    (evalDocState indentWidth (Comments.fromList comments) $ pretty x)
