module Lib where

import           Data.Text.Prettyprint.Doc.Util

import           Parser
import           Parser.Internal.FixityTable
import           Parser.DebugLevel
import           Lexer
import           Pretty
import qualified Pretty.Comments               as Comments

test :: (Show a) => Parser a -> DebugLevel -> Text -> IO ()
test parser debugLevel input = do
  putStrLn "Lexer: "
  lexTest input

  let Right (_, lexed) = runLexer "test" input
  let strm = stream "test" input lexed

  putStrLn ""
  putStrLn "Parser: "
  parseTest parser debugLevel strm

withBasis :: (FixityTable -> Parser a) -> Parser a
withBasis parser = parser basisFixityTable

testPretty :: (Show a, Pretty a)
           => Bool
           -> Parser a
           -> DebugLevel
           -> Int
           -> Text
           -> IO ()
testPretty verbose parser debugLevel w input = do
  when verbose $ do
    putStrLn "Lexer: "
    lexTest input

  let Right (comments, lexed) = runLexer "test" input
  let strm = stream "test" input lexed

  when verbose $ do
    putStrLn ""
    putStrLn "Parser: "
    parseTest parser debugLevel strm

  let Right parsed = runParser parser debugLevel "test" strm
  when verbose $ do
    putStrLn ""
    putStrLn "Formatted: "
  putDocW w (evalDocState 2 (Comments.fromList comments) (pretty parsed))
  putStrLn ""
