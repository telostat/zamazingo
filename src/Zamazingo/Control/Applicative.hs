-- | This module adds helper for alternative context.

module Zamazingo.Control.Applicative where

import Control.Applicative (Alternative(empty))
import Data.Bool           (bool)


-- | Lifts the given value into an 'Alternative' context depending on the
-- predicate.
--
-- Thanks to <https://hackage.haskell.org/package/protolude>
--
-- >>> guarded (const False) 1 :: Maybe Int
-- Nothing
-- >>> guarded (const True) 1 :: Maybe Int
-- Just 1
-- >>> guarded (const False) 1 :: [Int]
-- []
-- >>> guarded (const True) 1 :: [Int]
-- [1]
-- >>> guarded (const False) 1 :: IO Int
-- ...
-- ... user error (mzero)
-- ...
-- >>> guarded (const True) 1 :: IO Int
-- 1
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = bool empty (pure a) (p a)


-- | Lifts to given value into an 'Alternative' context with a predicate working
-- in 'Functor' context.
--
-- Thanks to <https://hackage.haskell.org/package/protolude>
--
-- >>> guardedA (const (pure False)) 1 :: Maybe (Maybe Int)
-- Just Nothing
-- >>> guardedA (const (pure True)) 1 :: Maybe (Maybe Int)
-- Just (Just 1)
-- >>> guardedA (const (pure False)) 1 :: [(Maybe Int)]
-- [Nothing]
-- >>> guardedA (const (pure True)) 1 :: [(Maybe Int)]
-- [Just 1]
-- >>> guardedA (const (pure False)) 1 :: Maybe [Int]
-- Just []
-- >>> guardedA (const (pure True)) 1 :: Maybe [Int]
-- Just [1]
-- >>> guardedA (const (pure False)) 1 :: IO (Maybe Int)
-- Nothing
-- >>> guardedA (const (pure True)) 1 :: IO (Maybe Int)
-- Just 1
-- >>> guardedA (const (pure False)) 1 :: IO [Int]
-- []
-- >>> guardedA (const (pure True)) 1 :: IO [Int]
-- [1]
guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA fb a = bool empty (pure a) <$> fb a
