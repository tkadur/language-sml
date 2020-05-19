module Language.Sml.Ast.Module.Signature.Decl where

import           Language.Sml.Ast.Ident.SignatureIdent
                                                ( MSignatureIdent )
import           Language.Sml.Ast.Module.Signature.Expr
                                                ( MExpr )
import           Language.Sml.Common.Marked     ( Marked )

type MDecl = Marked Decl

newtype Decl
  = Signature MBinds
  deriving (Eq, Show)

type MBinds = NonEmpty MBind

type MBind = Marked Bind

data Bind
  = Bind
    { ident :: MSignatureIdent
    , expr :: MExpr
    }
  deriving (Eq, Show)
