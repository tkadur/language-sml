{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty.Internal.Printers.Pattern where

import           Ast.Associativity
import qualified Ast.Associativity             as Associativity
import           Ast.Pat
import qualified Common.Marked                 as Marked
import           Pretty.Internal.Basic
import           Pretty.Internal.Printers.Identifier
                                                ( )
import           Pretty.Internal.Printers.Literal
                                                ( )

instance Pretty Pat where
  pretty = \case
    Wild         -> "_"
    Lit    lit   -> pretty lit
    Ident  ident -> pretty ident
    Record rows  -> record $ mapM pretty rows
    Tuple  pats  -> tupled $ mapM pretty pats
    List   pats  -> list $ mapM pretty pats

instance Pretty Row where
  pretty = \case
    RowWild -> "..."
    Row { label, pat } -> pretty label <+> equals <+> pretty pat
