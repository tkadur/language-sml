module Ast.Ident.LongIdent where

import           Ast.Ident.Ident                ( Ident )

-- | An identifier, possibly qualified
data LongIdent
    = Ident Ident
    | Qualified
        { qualifier :: Ident
        , ident :: LongIdent
        }
    deriving (Show)
