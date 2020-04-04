module Parser.Internal.Parsers.Toplevel where

import           Control.Monad.Combinators      ( many )

import           Ast.Decl                       ( MDecl )
import           Parser.Internal.Basic
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Declaration
                                                ( declaration )

-- | Parses the toplevel
toplevel :: Parser MDecl
toplevel =
  dbg ["toplevel"] $ evalStateT declaration FixityTable.basisFixityTable
