module Lexer
  ( Result
  , Error
  , runLexer
  , lexTest
  )
where

import           Common.Marked                  ( Marked )
import           Lexer.Internal
import           Lexer.Token                    ( Token )

type Result = ([Marked Comment], [Marked Token])
type Error = String

runLexer :: FilePath -> Text -> Either Error Result
runLexer filepath s = revComments <$> runAlex' filepath (toString s) lex
  where revComments (comments, tokens) = (reverse comments, tokens)

lexTest :: Text -> (Result -> IO ()) -> IO ()
lexTest s prnt = case runLexer "" s of
  Right res -> prnt res
  Left  err -> error $ toText err
