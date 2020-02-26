module Parser.Internal.Reserved where

class Reserved a where
  text :: a -> Text

data ReservedWord
    = Infix
    | Infixr
    | Val
    deriving (Show, Enum)

data ReservedOp
    = Equal
    deriving (Show, Enum)

instance Reserved ReservedWord where
  text = \case
    Infix  -> "infix"
    Infixr -> "infixr"
    Val    -> "val"

instance Reserved ReservedOp where
  text = \case
    Equal -> "="

reservedTokens :: [Text]
reservedTokens = reservedWords ++ reservedOps
 where
  reservedWords = text <$> enumFrom (toEnum 0 :: ReservedWord)
  reservedOps   = text <$> enumFrom (toEnum 0 :: ReservedOp)
