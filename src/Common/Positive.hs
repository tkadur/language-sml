module Common.Positive
  ( Positive()
  , positive
  , unPositive
  )
where

newtype Positive = Positive Integer
  deriving (Eq, Num, Ord, Show)

positive :: (Integral i) => i -> Positive
positive n | n <= 0    = error "input isn't positive"
           | otherwise = Positive (toInteger n)

unPositive :: Positive -> Integer
unPositive (Positive n) = n
