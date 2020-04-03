module Ast.Pat where

import           Ast.Associativity              ( Associativity )
import           Ast.Ident.Label                ( Label )
import           Ast.Ident.Long                 ( Long )
import           Ast.Ident.Op                   ( Op )
import           Ast.Ident.ValueIdent           ( ValueIdent )
import           Ast.Lit                        ( Lit )
import           Ast.Typ                        ( Typ )

data Pat
  = Wild
  | Lit Lit
  | Ident (Op (Long ValueIdent))
  | Record [Row]
  | Tuple [Pat]
  | List [Pat]
  | Constructed
    { constructor :: Op (Long ValueIdent)
    , arg :: Pat
    }
  | InfixConstructed
    { lhs :: Pat
    , op :: ValueIdent
    , precedence :: Int
    , associativity :: Associativity
    , rhs :: Pat
    }
  | Annot
    { pat :: Pat
    , typ :: Typ
    }
  | As
    { ident :: Op ValueIdent
    , annot :: Maybe Typ
    , as :: Pat
    }
  deriving (Eq, Show)

data Row
  = RowWild
  | Row
    { label :: Label
    , pat :: Pat
    }
  | RowPun
    { ident :: ValueIdent
    , annot :: Maybe Typ
    , as :: Maybe Pat
    }
  deriving (Eq, Show)
