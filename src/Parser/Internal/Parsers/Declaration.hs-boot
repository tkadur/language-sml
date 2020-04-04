module Parser.Internal.Parsers.Declaration where

import           Ast.Decl                       ( Decl )
import           Parser.Internal.Basic          ( MonadParser )
import           Parser.Internal.FixityTable    ( FixityTable )

declaration :: (MonadParser parser, MonadState FixityTable parser) => parser Decl