module Language.Sml.Ast.Module.Structure.Decl where

import qualified Language.Sml.Ast.Core.Decl    as Core.Decl
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( MStructureIdent )
import           Language.Sml.Ast.Module.Structure.Expr
                                                ( MExpr )
import           Language.Sml.Common.Marked     ( Marked )

type MDecl = Marked Decl

data Decl
  = CoreDecl Core.Decl.MDecl
  | Structure MBinds
  | Local
    { decl :: MDecl
    , body :: MDecl
    }
    -- For convenience, express sequence chains with a list instead of
    -- nested @MDecl@s
  | Sequence [MDecl]
  deriving (Eq, Show)

type MBinds = NonEmpty MBind

type MBind = Marked Bind

data Bind
  = Bind
    { ident :: MStructureIdent
    , expr :: MExpr
    }
  deriving (Eq, Show)
