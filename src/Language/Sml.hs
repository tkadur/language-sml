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

import           Language.Sml.Ast.Program       ( Program )
import           Language.Sml.Common.Marked     ( Marked )
import qualified Language.Sml.Lexer            as Lexer
import qualified Language.Sml.Lexer.Token      as Lexer.Token
import qualified Language.Sml.Parser           as Parser
import qualified Language.Sml.Parser.DebugLevel
                                               as DebugLevel
import qualified Language.Sml.Pretty           as Pretty
import qualified Language.Sml.Pretty.Comments  as Pretty.Comments

newtype Comments = Comments [Marked Lexer.Comment]
  deriving (Eq, Show)

data Lexed = Lexed FilePath Text [Marked Lexer.Token.Token]
  deriving (Eq, Show)

type Parsed = Program

lex :: FilePath -> Text -> Either Lexer.Error (Comments, Lexed)
lex filepath rawInput = case lexed of
  Right (comments, tokens) ->
    return (Comments comments, Lexed filepath rawInput tokens)
  Left err -> Left err
  where lexed = Lexer.runLexer filepath rawInput

parse :: Lexed -> Either Parser.Error Parsed
parse (Lexed filepath rawInput tokens) = case parsed of
  Right program -> return program
  Left  err     -> Left err
 where
  parsed = Parser.runParser Parser.program DebugLevel.Off filepath stream
  stream = Parser.stream filepath rawInput tokens

prettyPrint :: Pretty.Config -> Comments -> Program -> Text
prettyPrint config (Comments comments) =
  Pretty.prettyPrint config (Pretty.Comments.fromList comments)

reportLexerError :: Lexer.Error -> IO a
reportLexerError err = do
  putStrLn (Lexer.showError err)
  exitFailure

reportParserError :: Parser.Error -> IO a
reportParserError err = do
  putStrLn (Parser.showError err)
  exitFailure
