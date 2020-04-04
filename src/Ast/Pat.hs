module Ast.Pat where

import           Ast.Associativity              ( Associativity )
import           Ast.Ident.Label                ( MLabel )
import           Ast.Ident.Long                 ( MLong )
import           Ast.Ident.Op                   ( MOp )
import           Ast.Ident.ValueIdent           ( MValueIdent )
import           Ast.Lit                        ( Lit )
import           Ast.Typ                        ( MTyp )
import           Common.Marked                  ( Marked )

type MPat = Marked Pat

type MRow = Marked Row

data Pat
  = Wild
  | Lit Lit
  | Ident (MOp (MLong MValueIdent))
  | Record [MRow]
  | Tuple [MPat]
  | List [MPat]
  | Constructed
    { constructor :: MOp (MLong MValueIdent)
    , arg :: MPat
    }
  | InfixConstructed
    { lhs :: MPat
    , op :: MValueIdent
    , precedence :: Int
    , associativity :: Associativity
    , rhs :: MPat
    }
  | Annot
    { pat :: MPat
    , typ :: MTyp
    }
  | As
    { ident :: MOp MValueIdent
    , annot :: Maybe MTyp
    , as :: MPat
    }
  deriving (Eq, Show)

data Row
  = RowWild
  | Row
    { label :: MLabel
    , pat :: MPat
    }
  | RowPun
    { ident :: MValueIdent
    , annot :: Maybe MTyp
    , as :: Maybe MPat
    }
  deriving (Eq, Show)
