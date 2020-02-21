module Parser
    ( module Parser.Internal
    , Parser
    , module Parser
    )
where

import           Control.Monad.Writer.Strict    ( Writer )
import qualified Control.Monad.Writer.Strict   as Writer
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.Basic                   ( Parser )
import           Parser.Internal

parseTest :: (Show a) => Parser a -> Text -> IO ()
parseTest parser input = case result of
    Left  error  -> putStr (E.errorBundlePretty error)
    Right parsed -> print (parsed, comments)
  where
    (result, comments) = Writer.runWriter $ M.runParserT parser filename input
    filename           = ""
