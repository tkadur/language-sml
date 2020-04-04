module Lib where

import           Parser
import           Parser.DebugLevel
import           Lexer

test :: (Show a) => Parser a -> DebugLevel -> Text -> IO ()
test parser debugLevel input = do
  putStrLn "Lexer: "
  lexTest input

  let Right (_, lexed) = runLexer "test" input
  let strm = stream "test" input lexed

  putStrLn ""
  putStrLn "Parser: "
  parseTest parser debugLevel strm
