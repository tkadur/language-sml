module Parser
  ( module Parser.Internal
  , module Parser
  , Parser
  , DebugLevel(..)
  )
where

import qualified Control.Monad.RWS.Strict      as RWS
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.Internal
import           Parser.Internal.Basic          ( Parser
                                                , E
                                                , S
                                                , Comments
                                                , DebugLevel(..)
                                                )

runParser :: Parser a
          -> DebugLevel
          -> String
          -> S
          -> Either (M.ParseErrorBundle S E) (a, Comments)
runParser parser debugLevel filename input = (, comments) <$> result
 where
  (result, comments) =
    RWS.evalRWS (M.runParserT (parser <* M.eof) filename input) debugLevel ()

parseTest :: (Show a) => Parser a -> DebugLevel -> S -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err -> putStr (E.errorBundlePretty err)
    Right (parsed, comments) -> print (parsed, comments)
  where filename = ""
