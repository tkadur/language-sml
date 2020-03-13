module Ast.Ident.LongIdent where

import           Ast.Ident.Ident                ( Ident )

-- | An identifier, possibly qualified
data LongIdent
    = Ident Ident
    | Qualified
        { qualifiers :: NonEmpty Ident
        , ident :: Ident
        }
    deriving (Show)
