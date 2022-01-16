-- | Internal module for network port definition and related functionality.

module Zamazingo.Network.Internal.Port where

import           Control.Monad.Except (MonadError(throwError))
import qualified Data.Aeson           as Aeson
import           Data.Text            (Text, unpack)
import           Data.Word            (Word16)
import           Zamazingo.Text       (tshow)


-- | Type encoding of bounded network ports.
--
-- >>> minBound :: Port
-- 1
-- >>> maxBound :: Port
-- 65535
newtype Port = MkPort
  { unPort :: Word16
  }
  deriving (Eq, Ord)


-- | 'Show' instance for 'Port'
--
-- >>> minBound :: Port
-- 1
-- >>> maxBound :: Port
-- 65535
instance Show Port where
  show = show . unPort


-- | 'Bounded' instance for 'Port'
--
-- >>> minBound :: Port
-- 1
-- >>> maxBound :: Port
-- 65535
instance Bounded Port where
  minBound = MkPort 1
  maxBound = MkPort maxBound


-- | 'Aeson.FromJSON' instance for 'Port'.
--
-- >>> Aeson.eitherDecode "0" :: Either String Port
-- Left "Error in $: Ports are defined over the range of [1, 65535], but given: 0"
-- >>> Aeson.eitherDecode "1" :: Either String Port
-- Right 1
-- >>> Aeson.eitherDecode "65535" :: Either String Port
-- Right 65535
-- >>> Aeson.eitherDecode "65536" :: Either String Port
-- Left "Error in $: parsing Word16 failed, value is either floating or will cause over or underflow 65536.0"
-- >>> Aeson.eitherDecode "1.2" :: Either String Port
-- Left "Error in $: parsing Word16 failed, value is either floating or will cause over or underflow 1.2"
instance Aeson.FromJSON Port where
  parseJSON v = either (fail . unpack) pure . (mkPort @_ @Word16) =<< Aeson.parseJSON v


-- | 'Aeson.ToJSON' instance for 'Port'.
--
-- >>> Aeson.encode (minBound :: Port)
-- "1"
-- >>> Aeson.encode (maxBound :: Port)
-- "65535"
-- >>> Just (minBound :: Port) == Aeson.decode (Aeson.encode (minBound :: Port))
-- True
instance Aeson.ToJSON Port where
  toJSON = Aeson.Number . fromIntegral . unPort


-- | Smart constructor for 'Port' values.
--
-- The argument can be of any type that has a 'Integral' typeclass instance. The
-- function throws an error in case that the consumed parameter is not within
-- the expected interval.
--
-- >>> import Data.Word
-- >>> (,) <$> mkPort (1 :: Word16) <*> mkPort (65535 :: Word16) :: Either Text (Port, Port)
-- Right (1,65535)
-- >>> (,) <$> mkPort (1 :: Int) <*> mkPort (65535 :: Int) :: Either Text (Port, Port)
-- Right (1,65535)
-- >>> (,) <$> mkPort (1 :: Integer) <*> mkPort (65535 :: Integer) :: Either Text (Port, Port)
-- Right (1,65535)
-- >>> (,) <$> mkPort (1 :: Word8) <*> mkPort (255 :: Word8) :: Either Text (Port, Port)
-- Right (1,255)
-- >>> mkPort (0 :: Int) :: Either Text Port
-- Left "Ports are defined over the range of [1, 65535], but given: 0"
-- >>> mkPort (0 :: Word8) :: Either Text Port
-- Left "Ports are defined over the range of [1, 65535], but given: 0"
-- >>> mkPort (maxBound :: Int) :: Either Text Port
-- Left "Ports are defined over the range of [1, 65535], but given: 9223372036854775807"
mkPort :: (MonadError Text m, Integral a, Show a) => a -> m Port
mkPort p
  | p < minPortValue = err
  | p > maxPortValue = err
  | otherwise        = pure (MkPort (fromIntegral p))
  where
    minPortValue = fromIntegral (unPort minBound)
    maxPortValue = fromIntegral (unPort maxBound)
    err = throwError ("Ports are defined over the range of [" <> tshow (unPort minBound) <> ", " <> tshow (unPort maxBound) <> "], but given: " <> tshow p)
