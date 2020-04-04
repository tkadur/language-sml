module Lib where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util

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

testPretty :: (Show a, Pretty a)
           => Parser a
           -> DebugLevel
           -> Int
           -> Text
           -> IO ()
testPretty parser debugLevel w input = do
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
  putDocW w (pretty parsed)
  putStrLn ""
