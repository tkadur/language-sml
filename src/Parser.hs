module Parser
  ( module Parser
  , Parser
  , Stream
  , Internal.toplevel
  , Internal.declaration
  , Internal.expression
  , Internal.pattern
  , Stream.stream
  )
where

import qualified Control.Monad.RWS.Strict      as RWS
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E

import           Parser.DebugLevel              ( DebugLevel )
import           Parser.Internal               as Internal
import           Parser.Internal.Basic          ( Parser
                                                , Error
                                                , Comments
                                                , eof
                                                )
import           Parser.Internal.Stream         ( Stream )
import qualified Parser.Internal.Stream        as Stream

runParser :: Parser a
          -> DebugLevel
          -> String
          -> Stream
          -> Either (M.ParseErrorBundle Stream Error) (a, Comments)
runParser parser debugLevel filename input = (, comments) <$> result
 where
  (result, (), comments) =
    RWS.runRWS (M.runParserT (parser <* eof) filename input) debugLevel ()

parseTest :: (Show a) => Parser a -> DebugLevel -> Stream -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err -> putStr (E.errorBundlePretty err)
    Right (parsed, comments) -> print (parsed, comments)
  where filename = ""
