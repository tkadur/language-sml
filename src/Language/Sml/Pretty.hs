module Language.Sml.Pretty
  ( Pretty(..)
  , Config(..)
  , prettyPrint
  )
where

import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as Render


import           Language.Sml.Pretty.Comments   ( Comments )
import           Language.Sml.Pretty.Internal.Basic
                                                ( Pretty(..)
                                                , evalDocState
                                                )
import           Language.Sml.Pretty.Internal.Printers.Declaration
                                               as Declaration
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Expression
                                               as Expression
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Identifier
                                               as Identifier
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Literal
                                               as Literal
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Pattern
                                               as Pattern
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Type
                                               as Type
                                                ( )
import           Language.Sml.Pretty.Internal.Printers.Program
                                               as Program
                                                ( )

data Config =
  Config
  { lineLength :: Int
  , indentWidth :: Int
  }

prettyPrint :: (Pretty a) => Config -> Comments -> a -> Text
prettyPrint Config { lineLength, indentWidth } comments x =
  x
    |> pretty
    |> evalDocState indentWidth comments
    |> Doc.layoutPretty
         (Doc.LayoutOptions
           { Doc.layoutPageWidth = Doc.AvailablePerLine lineLength 1
           }
         )
    |> Render.renderStrict
    |> fixTrailingNewlines
 where
  -- Ensure that output ends with exactly one newline
  fixTrailingNewlines :: Text -> Text
  fixTrailingNewlines output = Text.dropWhileEnd (== '\n') output <> "\n"
