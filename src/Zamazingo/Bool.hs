-- | This module provides functions to work with boolean values and functions.

module Zamazingo.Bool where

import Data.Maybe (listToMaybe)


-- | Evaluates to 'True' if all predicates evaluate to 'True' for the given
-- argument.
--
-- >>> let not1 = (/=) 1
-- >>> let not2 = (/=) 2
-- >>> allp [not1, not2] 0
-- True
-- >>> allp [not1, not2] 1
-- False
-- >>> allp [not1, not2] 2
-- False
allp :: Foldable t => t (a -> Bool) -> a -> Bool
allp ps q = all ($ q) ps


-- | Evaluates to 'True' if any predicate evaluate to 'True' for the given
-- argument.
--
-- >>> let not1 = (/=) 1
-- >>> let not2 = (/=) 2
-- >>> anyp [not1, not2] 0
-- True
-- >>> anyp [not1, not2] 1
-- True
-- >>> anyp [not1, not2] 2
-- True
anyp :: Foldable t => t (a -> Bool) -> a -> Bool
anyp ps q = any ($ q) ps


-- | Checks if all elements of a given list are same.
--
-- >>> allEq []
-- True
-- >>> allEq @Int [1]
-- True
-- >>> allEq @Int [1, 1]
-- True
-- >>> allEq @Int [1, 2]
-- False
-- >>> allEq @Int [1, 2, undefined]
-- False
allEq :: Eq a => [a] -> Bool
allEq xs = maybe True (\x -> all (== x) xs) $ listToMaybe xs
