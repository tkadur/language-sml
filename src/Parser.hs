module Parser
  ( module Parser.Internal
  , Parser
  , module Parser
  )
where

import qualified Control.Monad.Writer.Strict   as Writer
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.Internal
import           Parser.Internal.Basic          ( Parser )

parseTest :: (Show a) => Parser a -> Text -> IO ()
parseTest parser input = case result of
  Left  err    -> putStr (E.errorBundlePretty err)
  Right parsed -> print (parsed, comments)
 where
  (result, comments) =
    Writer.runWriter $ M.runParserT (parser <* M.eof) filename input
  filename = ""
