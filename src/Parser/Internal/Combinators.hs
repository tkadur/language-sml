module Parser.Internal.Combinators where

import           Control.Monad.Combinators

-- | @sepBy2 p sep@ parses two or more occurrences of @p@, separated by @sep@.
sepBy2 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepBy2 parser sep = do
  e <- parser
  void sep
  es <- sepBy1 parser sep
  return $ e :| es
