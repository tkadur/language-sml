module Parser
  ( module Parser
  , Internal.topLevel
  , Internal.declaration
  , Internal.expression
  , Internal.pattern
  , Parser
  )
where

import qualified Control.Monad.RWS.Strict      as RWS
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.DebugLevel              ( DebugLevel )
import           Parser.Internal               as Internal
import           Parser.Internal.Basic          ( Parser
                                                , E
                                                , S
                                                , Comments
                                                )

runParser :: Parser a
          -> DebugLevel
          -> String
          -> S
          -> Either (M.ParseErrorBundle S E) (a, Comments)
runParser parser debugLevel filename input = (, comments) <$> result
 where
  (result, (), comments) =
    RWS.runRWS (M.runParserT (parser <* M.eof) filename input) debugLevel ()

parseTest :: (Show a) => Parser a -> DebugLevel -> S -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err -> putStr (E.errorBundlePretty err)
    Right (parsed, comments) -> print (parsed, comments)
  where filename = ""
