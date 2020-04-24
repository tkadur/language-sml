module Language.Sml.Parser.Internal.Parsers.Toplevel where

import           Control.Monad.Combinators      ( many )

import           Language.Sml.Ast.Toplevel      ( Toplevel(..) )
import           Language.Sml.Parser.Internal.Basic
import           Language.Sml.Parser.Internal.Parsers.Declaration
                                                ( declaration )

-- | Parses the toplevel
toplevel :: Parser Toplevel
toplevel = dbg ["toplevel"] (Toplevel <$> declaration)
