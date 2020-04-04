module Lib where

import           Data.Text.Prettyprint.Doc

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

testPretty :: (Show a, Pretty a) => Parser a -> DebugLevel -> Text -> IO ()
testPretty parser debugLevel input = do
  putStrLn "Lexer: "
  lexTest input

  let Right (_, lexed) = runLexer "test" input
  let strm = stream "test" input lexed

  putStrLn ""
  putStrLn "Parser: "
  parseTest parser debugLevel strm

  let Right parsed = runParser parser debugLevel "test" strm
  putStrLn ""
  putStrLn "Formatted: "
  print (pretty parsed)
