module Parser.Internal.Parsers.Declaration where

import           Ast.Decl                       ( MDecl )
import           Parser.Internal.Basic          ( MonadParser )
import           Parser.Internal.FixityTable    ( FixityTable )

declaration :: (MonadParser parser, MonadState FixityTable parser) => parser MDecl