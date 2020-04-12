module Language.Sml.Parser.Internal.Parsers.Toplevel where

import           Control.Monad.Combinators      ( many )

import           Language.Sml.Ast.Decl          ( MDecl )
import           Language.Sml.Parser.Internal.Basic
import qualified Language.Sml.Parser.Internal.FixityTable
                                               as FixityTable
import           Language.Sml.Parser.Internal.Parsers.Declaration
                                                ( declaration )

-- | Parses the toplevel
toplevel :: Parser MDecl
toplevel =
  dbg ["toplevel"] $ evalStateT declaration FixityTable.basisFixityTable
