module Language.Sml.Ast.Ident.SignatureIdent where

import           Language.Sml.Ast.Ident.Common  ( Alphanumeric )
import           Language.Sml.Common.Marked     ( Marked )

type MSignatureIdent = Marked SignatureIdent

-- | A structure name
newtype SignatureIdent = SignatureIdent Alphanumeric
  deriving (Eq, Show)
