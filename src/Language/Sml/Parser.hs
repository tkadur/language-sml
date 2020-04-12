module Language.Sml.Parser
  ( module Language.Sml.Parser
  , Parser
  , Stream
  , toplevel
  , declaration
  , expression
  , pattern
  , Stream.stream
  )
where

import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E
import           Text.Pretty.Simple             ( pPrint )

import           Language.Sml.Parser.DebugLevel ( DebugLevel )
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

runParser :: Parser a
          -> DebugLevel
          -> String
          -> Stream
          -> Either (M.ParseErrorBundle Stream Error) a
runParser parser debugLevel filename input =
  runReader (M.runParserT (parser <* eof) filename input) debugLevel

showError :: M.ParseErrorBundle Stream Error -> String
showError = E.errorBundlePretty

parseTest :: (Show a) => Parser a -> DebugLevel -> Stream -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err    -> putStr (E.errorBundlePretty err)
    Right parsed -> pPrint parsed
  where filename = ""
