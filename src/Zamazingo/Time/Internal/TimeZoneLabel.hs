-- | This module provides time-zone label and related definitions.
module Zamazingo.Time.Internal.TimeZoneLabel where

import           Control.Monad.Except (MonadError(throwError))
import qualified Data.Aeson           as Aeson
import qualified Data.Text.Encoding   as TE
import           Data.Time.Zones.DB   (TZLabel, fromTZName, toTZName)
import           Zamazingo.Text
                 ( TextCodec
                 , TextDecoder(decodeText)
                 , TextEncoder(encodeText)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding for time-zone labels.
--
-- This is a newtype wrapper around 'Data.Time.Zones.DB.TZLabel'.
--
-- >>> import qualified Zamazingo as Z
-- >>> Z.decodeText @TimeZoneLabel @(Either Data.Text.Text) "Europe/Istanbul"
-- Right (TimeZoneLabel {unTimeZoneLabel = Europe__Istanbul})
-- >>> Z.decodeText @TimeZoneLabel @(Either Data.Text.Text) "Asia/Istanbul"
-- Right (TimeZoneLabel {unTimeZoneLabel = Europe__Istanbul})
-- >>> Z.decodeText @TimeZoneLabel @(Either Data.Text.Text) "Africa/Istanbul"
-- Left "Unknown time-zone label value: Africa/Istanbul"
newtype TimeZoneLabel = TimeZoneLabel
  { unTimeZoneLabel :: TZLabel
  }
  deriving (Eq, Ord, Show)


instance TextCodec TimeZoneLabel


instance TextDecoder TimeZoneLabel where
  decodeText t = case fromTZName (TE.encodeUtf8 t) of
    Nothing -> throwError ("Unknown time-zone label value: " <> t)
    Just tz -> pure (TimeZoneLabel tz)


instance TextEncoder TimeZoneLabel where
  encodeText (TimeZoneLabel x) = TE.decodeUtf8 (toTZName x)


instance Aeson.FromJSON TimeZoneLabel where
  parseJSON = jsonDecoderFromTextDecoder "TimeZone"


instance Aeson.ToJSON TimeZoneLabel where
  toJSON = jsonEncoderFromTextEncoder
