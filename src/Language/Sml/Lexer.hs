module Language.Sml.Lexer
  ( Result
  , Error
  , Comment
  , runLexer
  , showError
  , lexTest
  )
where

import           Text.Pretty.Simple             ( pPrint )

import           Language.Sml.Common.Marked     ( Marked )
import           Language.Sml.Lexer.Internal
import           Language.Sml.Lexer.Token       ( Token )

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
