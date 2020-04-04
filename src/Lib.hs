module Lib where

import           Parser
import           Parser.DebugLevel
import           Lexer

test :: (Show a)
     => Parser a
     -> DebugLevel
     -> (forall b . Show b => b -> IO ())
     -> Text
     -> IO ()
test parser debugLevel prnt input = do
  putStrLn "Lexer: "
  lexTest input prnt

  let Right (_, lexed) = runLexer "test" input
  let strm = stream "test" input lexed

  putStrLn ""
  putStrLn "Parser: "
  parseTest parser debugLevel strm prnt
