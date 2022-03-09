-- | This module provides definition for type-safe identifier implemented via
-- phantom types.

module Zamazingo.Id where

import           Control.Exception   (Exception)
import           Control.Monad.Catch (MonadThrow(throwM))
import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)


-- | Type encoding for entity identifiers.
--
-- This encoding allows us to provide a phantom type for distinguishing between
-- identifiers of varying value types (@pt@) and an underlying identifier type
-- (@it@).
--
-- As an example, for given data definitions:
--
-- >>> data A
-- >>> data B
--
-- ... we can create identifiers specialised over these definitions individually:
--
-- >>> Id @A 1
-- Id {unId = 1}
-- >>> Id @B 1
-- Id {unId = 1}
--
-- Altough actual identifiers are same in /value/, they are not same and not compatible at the type-level:
--
-- > Id @A 1 == Id @B 1
-- Couldn't match type ‘B’ with ‘A’
-- Expected type: Id A Int
--   Actual type: Id B Int
--
-- ... because:
--
-- >>> :type Id @A 1
-- Id @A 1 :: Num it => Id A it
-- >>> :type Id @B 1
-- Id @B 1 :: Num it => Id B it
--
-- Values, on the otherhand, can be compared:
--
-- >>> unId (Id @A 1) == unId (Id @B 1)
-- True
--
-- as well as hashes:
--
-- >>> Data.Hashable.hash (Id @A 1) == Data.Hashable.hash (Id @B 1)
-- True
newtype Id pt it = Id { unId :: it }
  deriving(Eq, Ord, Hashable, Show)


-- | 'Aeson.FromJSON' instance for 'Id'.
--
-- >>> Aeson.decode @(Id Int Int) "1"
-- Just (Id {unId = 1})
-- >>> Aeson.decode @(Id String Int) "1"
-- Just (Id {unId = 1})
--
-- ... where:
--
-- >>> :type (Aeson.decode @(Id Int Int) "1")
-- (Aeson.decode @(Id Int Int) "1") :: Maybe (Id Int Int)
-- >>> :type (Aeson.decode @(Id String Int) "1")
-- (Aeson.decode @(Id String Int) "1") :: Maybe (Id String Int)
instance (Aeson.FromJSON it) => Aeson.FromJSON (Id pt it) where
  parseJSON = fmap Id . Aeson.parseJSON


-- | 'Aeson.ToJSON' instance for 'Id'.
--
-- >>> Aeson.encode (Id @Int 1)
-- "1"
-- >>> Aeson.encode (Id @String 1)
-- "1"
instance (Aeson.ToJSON it) => Aeson.ToJSON (Id pt it) where
  toJSON (Id x) = Aeson.toJSON x


-- | Type encoding for a lookup table from entity 'Id's to corresponding entities.
--
-- /Note: I think that this is quite dumb./
--
-- >>> data A = A { aId :: Int } deriving Show
-- >>> let values = [A 1, A 2, A 3]
-- >>> let table = HM.fromList (fmap (\x -> (Id @A (aId x), x)) values) :: IdLookup A Int
-- >>> HM.lookup (Id @A 1) table
-- Just (A {aId = 1})
type IdLookup pt it = HM.HashMap (Id pt it) pt


-- | Creates a lookup table from given values using the given key function.
lookupTableFromValues
  :: Eq it
  => Hashable it
  => (pt -> Id pt it)
  -> [pt]
  -> IdLookup pt it
lookupTableFromValues f = lookupTableFromArbitraryValues f id


-- | Creates a lookup table from given arbitrary values using the given key
-- function and value functions.
lookupTableFromArbitraryValues
  :: Eq it
  => Hashable it
  => (p -> Id pt it)
  -> (p -> pt)
  -> [p]
  -> IdLookup pt it
lookupTableFromArbitraryValues fK fV = HM.fromList . fmap ((,) <$> fK <*> fV)


-- | Attempts to lookup a given identifier in the given lookup table. If the
-- lookup fails, throws provided exception.
lookupIdThrow
  :: Eq it
  => Hashable it
  => Exception e
  => MonadThrow m
  => e
  -> Id pt it
  -> IdLookup pt it
  -> m pt
lookupIdThrow exc ident = maybe (throwM exc) pure . HM.lookup ident
