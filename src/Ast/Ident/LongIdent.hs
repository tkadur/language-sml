module Ast.Ident.LongIdent where

import qualified Ast.Ident.Ident               as Ident

-- | An identifier, possibly qualified
data LongIdent
    = Ident Ident.Untagged
    | Qualified
        { qualifiers :: NonEmpty (Ident.Tagged 'Ident.Alphanumeric)
        , ident :: Ident.Untagged
        }
    deriving (Show)
