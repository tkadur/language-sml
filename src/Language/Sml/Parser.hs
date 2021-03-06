module Language.Sml.Parser
  ( Parser
  , Error
  , parseTest
  , showError
  , runParser
  , program
  , declaration
  , expression
  , pattern
  , typ
  , stream
  )
where

import qualified Control.Monad.RWS.Strict      as RWS
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Error         as E
import           Text.Pretty.Simple             ( pPrint )

import           Language.Sml.Parser.DebugLevel ( DebugLevel )
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import           Language.Sml.Parser.Internal.Parsers.Declaration
                                                ( declaration )
import           Language.Sml.Parser.Internal.Parsers.Expression
                                                ( expression )
import           Language.Sml.Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Language.Sml.Parser.Internal.Parsers.Program
                                                ( program )
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
runParser parser debugLevel filename input = res
 where
  (res, ()) = RWS.evalRWS (M.runParserT (parser <* eof) filename input)
                          debugLevel
                          -- Start off with the infix operators provided by the basis
                          FixityTable.basisFixityTable
