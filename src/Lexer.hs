module Lexer
  ( Result
  , Error
  , Comment
  , runLexer
  , showError
  , lexTest
  )
where

import           Text.Pretty.Simple             ( pPrint )

import           Common.Marked                  ( Marked )
import           Lexer.Internal
import           Lexer.Token                    ( Token )

type Result = ([Marked Comment], [Marked Token])
type Error = String

runLexer :: FilePath -> Text -> Either Error Result
runLexer filepath s = revComments <$> runAlex' filepath (toString s) lex
  where revComments (comments, tokens) = (reverse comments, tokens)

showError :: Error -> String
showError = id

lexTest :: Text -> IO ()
lexTest s = case runLexer "" s of
  Right res -> pPrint res
  Left  err -> error $ toText err
