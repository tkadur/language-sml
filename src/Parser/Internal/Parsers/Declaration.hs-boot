module Parser.Internal.Parsers.Declaration where

import           Ast.Decl                       ( Decl )
import           Parser.Internal.Basic          ( Parser )
import           Parser.Internal.FixityTable    ( FixityTable )

declaration :: StateT FixityTable Parser Decl