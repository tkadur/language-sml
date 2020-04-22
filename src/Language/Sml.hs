module Language.Sml
  ( Comments
  , Lexed
  , Parsed
  , lex
  , parse
  , prettyPrint
  , reportLexerError
  , reportParserError
  )
where

import           Language.Sml.Ast.Toplevel      ( Toplevel )
import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Lexer            as Lexer
import qualified Language.Sml.Lexer.Token      as Lexer.Token
import qualified Language.Sml.Parser           as Parser
import qualified Language.Sml.Parser.DebugLevel
                                               as DebugLevel
import qualified Language.Sml.Pretty           as Pretty

newtype Comments = Comments [Marked Lexer.Comment]
  deriving (Eq, Show)

data Lexed = Lexed FilePath Text [Marked Lexer.Token.Token]
  deriving (Eq, Show)

type Parsed = Toplevel

lex :: FilePath -> Text -> Either Lexer.Error (Comments, Lexed)
lex filepath rawInput = case lexed of
  Right (comments, tokens) ->
    return (Comments comments, Lexed filepath rawInput tokens)
  Left err -> Left err
  where lexed = Lexer.runLexer filepath rawInput

parse :: Lexed -> Either Parser.Error Parsed
parse (Lexed filepath rawInput tokens) = case parsed of
  Right toplevel -> return toplevel
  Left  err      -> Left err
 where
  parsed = Parser.runParser Parser.toplevel DebugLevel.Off filepath stream
  stream = Parser.stream filepath rawInput tokens

prettyPrint :: Pretty.Config -> Comments -> Toplevel -> Text
prettyPrint config (Comments comments) toplevel =
  Pretty.prettyPrint config comments toplevel

reportLexerError :: Lexer.Error -> IO a
reportLexerError err = do
  putStrLn (Lexer.showError err)
  exitFailure

reportParserError :: Parser.Error -> IO a
reportParserError err = do
  putStrLn (Parser.showError err)
  exitFailure
