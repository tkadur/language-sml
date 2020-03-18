module Parser.Internal.Parsers.Toplevel where

import           Control.Monad.Combinators      ( many )

import           Ast.Decl                       ( Decl )
import           Parser.Internal.Basic
import qualified Parser.Internal.FixityTable   as FixityTable
import           Parser.Internal.Parsers.Declaration
                                                ( declaration )

-- | Parses the toplevel
toplevel :: Parser [Decl]
toplevel = dbg ["toplevel"] $ evalStateT decls FixityTable.basisFixityTable
  where decls = many declaration
