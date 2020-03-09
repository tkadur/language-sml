module Ast.Ident.ValueIdent where

import           Ast.Ident.LongIdent            ( LongIdent )

-- | An identifier, possibly with qualifications and "op" prefixes
data ValueIdent
    = LongIdent LongIdent
    | Op ValueIdent
    deriving (Show)
