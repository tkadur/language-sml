module Ast.Decl where

import           Ast.Expr                       ( Expr )
import           Ast.Pat                        ( Pat )
import           Ast.Ident.Ident                ( Ident )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Typ                        ( Typ
                                                , TyVar
                                                )

data Decl
  = Val
    { tyvars :: [TyVar]
    , lhs :: Pat
    , rhs :: Expr
    }
  | ValRec
    { tyVars :: [TyVar]
    , lhs :: Pat
    , rhs :: Expr
    }
  | ValAnd
    { tyVars :: [TyVar]
    , lhs :: Pat
    , rhs :: Expr
    }
  | Fun
    { clauses :: [FunClause]
    }
  | FunAnd
    { clauses :: [FunClause]
    }
  -- TODO(tkadur) the rest
  | Infix
    { precedence :: Maybe Int
    , idents :: NonEmpty Ident
    }
  | Infixr
    { precedence :: Maybe Int
    , idents :: NonEmpty Ident
    }
  | Nonfix
    { idents :: NonEmpty Ident
    }
  deriving (Show)

-- | Function declaration clause
data FunClause
  = InfixClause
    { name :: ValueIdent
    , args :: NonEmpty Pat
    , returnType :: Maybe Typ
    , body :: Expr
    }
  | NonfixClause
    { name :: ValueIdent
    , args :: NonEmpty Pat
    , returnType :: Maybe Typ
    , body :: Expr
    }
  deriving (Show)
