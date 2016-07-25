-- | Various utilities use by Shift that do not belong in any specific module.
module Shift.Utilities where

import Control.Exception (Exception)

import Control.Monad.Catch (MonadThrow, throwM)

-- | Attempts to extract a value from a Maybe type. If it fails, the provided
-- exception is throw.
--
-- This works like $fromJust$, but on a MonadThrow context.
orError :: (Exception e, MonadThrow m) => Maybe a -> e -> m a
orError Nothing x = throwM x
orError (Just x) _ = pure x

orThrow :: (Exception e, MonadThrow m) => Either e a -> m a
orThrow (Left x) = throwM x
orThrow (Right x) = pure x

pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)
