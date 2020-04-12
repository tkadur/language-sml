module Language.Sml.Parser
  ( Parser
  , parseToplevel
  , parseTest
  , showError
  , runParser
  )
where

import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E
import           Text.Pretty.Simple             ( pPrint )

import           Language.Sml.Ast.Toplevel      ( Toplevel )
import qualified Language.Sml.Lexer.Token      as Lexer.Token
import           Language.Sml.Common.Marked     ( Marked )
import           Language.Sml.Parser.DebugLevel ( DebugLevel )
import qualified Language.Sml.Parser.DebugLevel
                                               as DebugLevel
import           Language.Sml.Parser.Internal.Parsers.Declaration
                                                ( declaration )
import           Language.Sml.Parser.Internal.Parsers.Expression
                                                ( expression )
import           Language.Sml.Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Language.Sml.Parser.Internal.Parsers.Toplevel
                                                ( toplevel )
import           Language.Sml.Parser.Internal.Basic
                                                ( Parser
                                                , Error
                                                , eof
                                                )
import           Language.Sml.Parser.Internal.Stream
                                                ( Stream )
import qualified Language.Sml.Parser.Internal.Stream
                                               as Stream

parseToplevel :: FilePath
              -> Text
              -> [Marked Lexer.Token.Token]
              -> Either (M.ParseErrorBundle Stream Error) Toplevel
parseToplevel filename input tokens = runParser toplevel
                                                DebugLevel.Off
                                                filename
                                                stream
  where stream = Stream.stream filename input tokens

showError :: M.ParseErrorBundle Stream Error -> String
showError = E.errorBundlePretty

parseTest :: (Show a) => Parser a -> DebugLevel -> Stream -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err    -> putStr (E.errorBundlePretty err)
    Right parsed -> pPrint parsed
  where filename = ""

runParser :: Parser a
          -> DebugLevel
          -> FilePath
          -> Stream
          -> Either (M.ParseErrorBundle Stream Error) a
runParser parser debugLevel filename input =
  runReader (M.runParserT (parser <* eof) filename input) debugLevel
