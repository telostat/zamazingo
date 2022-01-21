-- | This internal module provides monadic helpers.

module Zamazingo.Control.Monad where

import Control.Monad (MonadPlus, guard, unless, when)


-- | 'Control.Monad.when' with monadic predicate.
--
-- >>> whenM (pure True) (putStrLn "True")
-- True
-- >>> whenM (pure False) (putStrLn "True")
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= flip when ma


-- | 'Control.Monad.unless' with monadic predicate.
--
-- >>> unlessM (pure True) (putStrLn "True")
-- >>> unlessM (pure False) (putStrLn "False")
-- False
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = mb >>= flip unless ma


-- | Monadic if-then-else.
--
-- >>> ifM (pure True) (pure "True") (pure "False")
-- "True"
-- >>> ifM (pure False) (pure "True") (pure "False")
-- "False"
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb maT maF = mb >>= \b -> if b then maT else maF


-- | 'guard' with monadic predicate.
--
-- >>> guardM (pure False) :: Maybe ()
-- Nothing
-- >>> guardM (pure True) :: Maybe ()
-- Just ()
-- >>> guardM (pure False) >> pure 1 :: Maybe Int
-- Nothing
-- >>> guardM (pure True) >> pure 1 :: Maybe Int
-- Just 1
guardM :: MonadPlus m => m Bool -> m ()
guardM mb = mb >>= guard
