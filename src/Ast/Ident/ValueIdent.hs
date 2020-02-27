module Ast.Ident.ValueIdent where

import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.LongIdent            ( LongIdent )
import qualified Ast.Ident.LongIdent           as LongIdent

-- | An identifier, possibly with qualifications and "op" prefixes
data ValueIdent
    = LongIdent LongIdent
    | Op ValueIdent
    deriving (Show)

fromIdent :: Ident -> ValueIdent
fromIdent = LongIdent . LongIdent.Ident
