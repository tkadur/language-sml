module Parser.Marked where

import qualified Text.Megaparsec.Pos           as P

data Marked a = Marked
    { value :: a
    , sourcePos :: P.SourcePos
    }
    deriving (Functor, Show)
