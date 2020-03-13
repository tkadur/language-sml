module Prelude
  ( module Relude
  , module Prelude
  )
where

import           Relude                  hiding ( Op
                                                , many
                                                , some
                                                )

type ShowS = String -> String

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 <<
(<<) :: (Monad m) => m a -> m b -> m a
m1 << m2 = do
  x <- m1
  m2
  return x

enumerate :: (Num i, Enum i) => [a] -> [(i, a)]
enumerate = zip [0 ..]

mapi :: (Num i, Enum i) => (i -> a -> b) -> [a] -> [b]
mapi f xs = map (uncurry f) (enumerate xs)

update :: (Num i, Enum i, Eq i) => i -> (a -> a) -> [a] -> [a]
update i f = mapi (\i' x -> if i == i' then f x else x)

-- | @dropUntil f xs@ drops elements of @xs@ until the suffix satisfies @f@
dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil f xs = case (f xs, xs) of
  (True , _      ) -> xs
  (_    , []     ) -> xs
  (False, _ : xs') -> dropUntil f xs'

-- | @foldingMap@ is a version of @map@ that threads an accumulator
--   through calls to @f@
foldingMap :: (b -> a -> (b, c)) -> b -> [a] -> [c]
foldingMap f z xs = case xs of
  []      -> []
  x : xs' -> let (z', x') = f z x in x' : foldingMap f z' xs'
