module Language.Sml.Parser.Internal.Parsers.Program where

import           Control.Monad.Combinators      ( many )

import           Language.Sml.Ast.Program       ( Program(..) )
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.Parsers.Declaration
                                                ( declaration )

-- | Parses the program
program :: Parser Program
program = dbg ["program"] (Program <$> declaration)
