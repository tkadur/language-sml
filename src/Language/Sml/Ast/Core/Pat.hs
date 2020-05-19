module Language.Sml.Ast.Core.Pat where

import           Language.Sml.Ast.Associativity ( Associativity )
import           Language.Sml.Ast.Core.Ident.Label
                                                ( MLabel )
import           Language.Sml.Ast.Core.Ident.Long
                                                ( MLong )
import           Language.Sml.Ast.Core.Ident.Op ( MOp )
import           Language.Sml.Ast.Core.Ident.ValueIdent
                                                ( MValueIdent )
import           Language.Sml.Ast.Core.Lit      ( Lit )
import           Language.Sml.Ast.Core.Typ      ( MTyp )
import           Language.Sml.Common.Marked     ( Marked )

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
