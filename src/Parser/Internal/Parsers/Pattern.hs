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
  pattern'    = choice [wild, lit, constructed, var, tup, lst, parens]

  -- Wildcard
  wild        = dbg ["pattern", "wild"] $ Pat.Wild <$ token_ Token.Underscore

  -- Literal
  lit         = dbg ["pattern", "lit"] $ Pat.Lit <$> literal

  -- Constructor application
  -- @try@ to prevent failure from trying to parse variable as constructor
  constructed = try $ do
    constructor <- nonfixValueIdentifier fixityTable
    arg         <- pattern fixityTable
    return Pat.Constructed { Pat.constructor, Pat.arg }

  -- Variable
  var = dbg ["pattern", "var"]
    -- @try@ to prevent failure from trying to parse infix operator as var
    $ try (Pat.Ident <$> nonfixValueIdentifier fixityTable)

  -- Tuple
  tup    = Pat.Tuple <$> tuple (pattern fixityTable)

  -- List
  lst    = Pat.List <$> list (pattern fixityTable)

  -- Parenthesized
  -- @try@ to prevent failure from consuming the start of a tuple
  parens = try $ parenthesized (pattern fixityTable)
