module Parser
  ( module Parser
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

import           Parser.DebugLevel              ( DebugLevel )
import           Parser.Internal.Parsers.Declaration
                                                ( declaration )
import           Parser.Internal.Parsers.Expression
                                                ( expression )
import           Parser.Internal.Parsers.Pattern
                                                ( pattern )
import           Parser.Internal.Parsers.Toplevel
                                                ( toplevel )
import           Parser.Internal.Basic          ( Parser
                                                , Error
                                                , eof
                                                )
import           Parser.Internal.Stream         ( Stream )
import qualified Parser.Internal.Stream        as Stream

runParser :: Parser a
          -> DebugLevel
          -> String
          -> Stream
          -> Either (M.ParseErrorBundle Stream Error) a
runParser parser debugLevel filename input =
  runReader (M.runParserT (parser <* eof) filename input) debugLevel

parseTest :: (Show a) => Parser a -> DebugLevel -> Stream -> IO ()
parseTest parser debugLevel input =
  case runParser parser debugLevel filename input of
    Left  err    -> putStr (E.errorBundlePretty err)
    Right parsed -> pPrint parsed
  where filename = ""
