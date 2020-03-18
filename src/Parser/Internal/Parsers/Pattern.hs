module Parser.Internal.Parsers.Pattern where

import           Control.Monad.Combinators      ( choice )
import           Text.Megaparsec                ( try )

import           Ast.Pat                        ( Pat )
import qualified Ast.Pat                       as Pat
import           Parser.Internal.Basic
import           Parser.Internal.FixityTable    ( FixityTable )
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Identifier
                                                ( nonfixValueIdentifier )
import           Parser.Internal.Parsers.Literal
                                                ( literal )
import qualified Parser.Internal.Token         as Token

-- | Parses a pattern
pattern :: FixityTable -> Parser Pat
pattern fixityTable = dbg ["pattern"]
  $ FixityTable.makeParser FixityTable.Pat pattern' fixityTable
 where
  pattern' = choice [wild, lit, var, row, tup, lst, parens, annot, as]

  -- Wildcard
  wild     = dbg ["pattern", "wild"] $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit      = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- Variable
  var =
    -- @try@ to prevent failure from trying to parse infix operator as bareIdentifier
    dbg ["pattern", "var"] $ try (Pat.Var <$> nonfixValueIdentifier fixityTable)

  -- Record
  row    = empty

  -- Tuple
  tup    = Pat.Tuple <$> tuple (pattern fixityTable)

  -- List
  lst    = Pat.List <$> list (pattern fixityTable)

  -- Parenthesized
  -- @try@ to prevent failure from consuming the start of a tuple
  parens = try $ parenthesized (pattern fixityTable)

  -- Annotated
  annot  = empty

  -- As-pattern
  as     = empty
