module Parser.Internal.Reserved where

class Reserved a where
  text :: a -> Text

data ReservedWords
    = Infix
    | Infixr
    | Val
    deriving (Show, Enum)

data ReservedOps
    = Equal
    deriving (Show, Enum)

instance Reserved ReservedWords where
  text = \case
    Infix  -> "infix"
    Infixr -> "infixr"
    Val    -> "val"

instance Reserved ReservedOps where
  text = \case
    Equal -> "="

reservedTokens :: [Text]
reservedTokens = reservedWords ++ reservedOps
 where
  reservedWords = text <$> enumFrom (toEnum 0 :: ReservedWords)
  reservedOps   = text <$> enumFrom (toEnum 0 :: ReservedOps)
