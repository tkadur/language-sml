module Language.Sml.Parser.Internal.Parsers.Declaration where

import           Language.Sml.Ast.Decl                       ( MDecl )
import           Language.Sml.Parser.Internal.Basic          ( MonadParser )
import           Language.Sml.Parser.Internal.FixityTable    ( FixityTable )

declaration :: (MonadParser parser, MonadState FixityTable parser) => parser MDecl