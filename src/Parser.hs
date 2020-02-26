module Parser
  ( module Parser.Internal
  , module Parser
  , Parser
  )
where

import qualified Control.Monad.Writer.Strict   as Writer
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.Internal
import           Parser.Internal.Basic          ( Parser
                                                , E
                                                , S
                                                , Comments
                                                )

runParser :: Parser a
          -> String
          -> S
          -> Either (M.ParseErrorBundle S E) (a, Comments)
runParser parser filename input = (, comments) <$> result
 where
  (result, comments) =
    Writer.runWriter $ M.runParserT (parser <* M.eof) filename input

parseTest :: (Show a) => Parser a -> S -> IO ()
parseTest parser input = case runParser parser filename input of
  Left  err -> putStr (E.errorBundlePretty err)
  Right (parsed, comments) -> print (parsed, comments)
  where filename = ""
