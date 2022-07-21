-- | This module provides functions to group a traversable.

module Zamazingo.GroupBy where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)
import qualified Data.List.NonEmpty  as NE


-- | Groups a foldable of elements in a key-value dictionary. The key is
-- provided as a function over the element.
--
-- The order of the list in the resulting 'NE.NonEmpty' are in reverse order
-- compared to the original foldable.
--
-- >>> groupBy []
-- fromList []
-- >>> groupBy [('a', 1), ('b', 2), ('a', 3), ('b', 4)]
-- fromList [('a',3 :| [1]),('b',4 :| [2])]
groupBy
  :: Eq k
  => Hashable k
  => Foldable f
  => f (k, v)
  -> HM.HashMap k (NE.NonEmpty v)
groupBy = foldl (flip groupByAux) HM.empty


-- | Groups a foldable of elements in a key-value dictionary. The key is
-- provided as a function over the element.
--
-- The order of elements in the resulting 'NE.NonEmpty' are in reverse order
-- compared to the original input.
--
-- >>> groupByFK id []
-- fromList []
-- >>> groupByFK (`mod` 2) [1, 2, 3, 4]
-- fromList [(0,4 :| [2]),(1,3 :| [1])]
-- >>> groupByFK (`mod` 4) [1, 2, 3, 4]
-- fromList [(0,4 :| []),(1,1 :| []),(2,2 :| []),(3,3 :| [])]
groupByFK
  :: Eq k
  => Hashable k
  => Functor f
  => Foldable f
  => (v -> k)
  -> f v
  -> HM.HashMap k (NE.NonEmpty v)
groupByFK fk = groupByFKV fk id


-- | Groups a foldable of elements in a key-value dictionary. The key and value are
-- provided as functions over the element.
--
-- The order of elements in the resulting 'NE.NonEmpty' are in reverse order
-- compared to the original input.
--
-- >>> groupByFKV id id []
-- fromList []
-- >>> groupByFKV (`mod` 2) (1 +) [1, 2, 3, 4]
-- fromList [(0,5 :| [3]),(1,4 :| [2])]
-- >>> groupByFKV (`mod` 4) (1 +) [1, 2, 3, 4]
-- fromList [(0,5 :| []),(1,2 :| []),(2,3 :| []),(3,4 :| [])]
groupByFKV
  :: Eq k
  => Hashable k
  => Functor f
  => Foldable f
  => (a -> k)
  -> (a -> v)
  -> f a
  -> HM.HashMap k (NE.NonEmpty v)
groupByFKV fk fv = groupBy . fmap ((,) <$> fk <*> fv)


-- | Auxiliary function to 'groupByFunc'.
--
-- >>> groupByAux ("a", 1) (HM.empty)
-- fromList [("a",1 :| [])]
-- >>> groupByAux ("b", 2) (HM.fromList [("a", 1 NE.:| [])])
-- fromList [("b",2 :| []),("a",1 :| [])]
-- >>> groupByAux ("a", 3) (HM.fromList [("b",2 NE.:| []),("a",1 NE.:| [])])
-- fromList [("b",2 :| []),("a",3 :| [1])]
groupByAux
  :: Eq k
  => Hashable k
  => (k, v)
  -> HM.HashMap k (NE.NonEmpty v)
  -> HM.HashMap k (NE.NonEmpty v)
groupByAux (k, v) d =
  let
    g = case HM.lookup k d of
      Nothing -> v NE.:| []
      Just ne -> v NE.<| ne
  in
    HM.insert k g d
