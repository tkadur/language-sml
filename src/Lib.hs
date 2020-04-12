module Lib where

import           Language.Sml.Parser
import           Language.Sml.Parser.Internal.FixityTable
import           Language.Sml.Parser.DebugLevel
import           Language.Sml.Lexer
import           Language.Sml.Pretty
import qualified Language.Sml.Pretty.Comments  as Comments

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
  putTextLn $ prettyPrint (Config { lineLength = w, indentWidth = 2 })
                          (Comments.fromList comments)
                          parsed
  putStrLn ""
