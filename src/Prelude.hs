module Prelude
       ( module Relude
       , module Prelude
       )
where

import           Relude                  hiding ( Op )
import qualified Relude.Unsafe                 as Unsafe

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

enumerate :: (Num i, Enum i) => [a] -> [(i, a)]
enumerate = zip [0 ..]

mapi :: (Num i, Enum i) => (i -> a -> b) -> [a] -> [b]
mapi f xs = uncurry f <$> enumerate xs

update :: (Num i, Enum i, Eq i) => i -> (a -> a) -> [a] -> [a]
update i f = mapi (\i' x -> if i == i' then f x else x)

foldl1' :: (Foldable t) => (a -> a -> a) -> t a -> a
foldl1' f xs = Unsafe.fromJust $ foldl' f' Nothing xs
   where
      f' acc x = Just $ case acc of
             Nothing -> x
             Just y  -> f y x
