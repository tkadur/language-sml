module Language.Sml.Ast.Module.Structure.Expr where

import qualified Language.Sml.Ast.Module.Signature.Expr
                                               as Signature.Expr
import {-# SOURCE #-} Language.Sml.Ast.Module.Structure.Decl
                                                ( MDecl )
import           Language.Sml.Ast.Ident.Long    ( MLong )
import           Language.Sml.Ast.Ident.FunctorIdent
                                                ( MFunctorIdent )
import           Language.Sml.Ast.Ident.StructureIdent
                                                ( MStructureIdent )
import           Language.Sml.Common.Marked     ( Marked )

type MExpr = Marked Expr

data Expr
  = Struct MDecl
  | Ident (MLong MStructureIdent)
  | TransparentAscription
    { struct :: MExpr
    , sig :: Signature.Expr.MExpr
    }
  | OpaqueAscription
    { struct :: MExpr
    , sig :: Signature.Expr.MExpr
    }
  | FunctorApp
    { functor :: MFunctorIdent
    , struct :: MExpr
    }
  | Let
    { decl :: MDecl
    , expr :: MExpr
    }
  deriving (Eq, Show)

