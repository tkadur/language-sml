module Language.Sml.Parser
  ( Parser
  , Error
  , parseTest
  , showError
  , runParser
  , toplevel
  , declaration
  , expression
  , pattern
  , typ
  , stream
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
import           Language.Sml.Parser.Internal.Parsers.Type
                                                ( typ )
import           Language.Sml.Parser.Internal.Basic
                                                ( Parser
                                                , eof
                                                )
import qualified Language.Sml.Parser.Internal.Basic
                                               as Basic
import           Language.Sml.Parser.Internal.Stream
                                                ( Stream
                                                , stream
                                                )

type Error = M.ParseErrorBundle Stream Basic.Error

showError :: Error -> String
showError = E.errorBundlePretty

parseTest :: (Show a) => Parser a -> DebugLevel -> Stream -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err    -> putStr (E.errorBundlePretty err)
    Right parsed -> pPrint parsed
  where filename = ""

runParser :: Parser a -> DebugLevel -> FilePath -> Stream -> Either Error a
runParser parser debugLevel filename input =
  runReader (M.runParserT (parser <* eof) filename input) debugLevel
