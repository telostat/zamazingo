-- | This internal provides the definitions for a date range data type and
-- related functions.

module Zamazingo.Time.Internal.DateRange where

import           Control.Monad.Except (MonadError(throwError), when)
import           Data.Aeson           ((.:), (.=))
import qualified Data.Aeson           as Aeson
import           Data.Text            (Text, unpack)
import qualified Data.Time            as Date.Time
import           Zamazingo.Text       (tshow)


-- | Type encoding for a date range.
--
-- @until@ is strictly greater than @since@.
data DateRange = MkDateRange
  { dateRangeSince :: !Date.Time.Day
  , dateRangeUntil :: !Date.Time.Day
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'DateRange'.
--
-- >>> Aeson.eitherDecode @DateRange "{\"since\": \"2021-01-01\", \"until\": \"2021-12-31\"}"
-- Right (MkDateRange {dateRangeSince = 2021-01-01, dateRangeUntil = 2021-12-31})
-- >>> Aeson.eitherDecode @DateRange "{\"since\": \"2021-01-02\", \"until\": \"2021-01-01\"}"
-- Left "Error in $: Invalid date range (since > until): 2021-01-02 > 2021-01-01"
instance Aeson.FromJSON DateRange where
  parseJSON = Aeson.withObject "DateRange" $ \o -> do
    sinceDate <- o .: "since"
    untilDate <- o .: "until"
    either (fail . unpack) pure (mkDateRange sinceDate untilDate)


-- | 'Aeson.FromJSON' instance for 'DateRange'.
--
-- >>> let dateRange = MkDateRange (read "2021-01-01") (read "2021-12-31")
-- >>> Aeson.encode dateRange
-- "{\"since\":\"2021-01-01\",\"until\":\"2021-12-31\"}"
-- >>> Aeson.eitherDecode @DateRange (Aeson.encode dateRange)
-- Right (MkDateRange {dateRangeSince = 2021-01-01, dateRangeUntil = 2021-12-31})
-- >>> (Right dateRange) == Aeson.eitherDecode @DateRange (Aeson.encode dateRange)
-- True
instance Aeson.ToJSON DateRange where
  toJSON (MkDateRange s u) = Aeson.object
    [ "since" .= s
    , "until" .= u
    ]


-- | Smart constructor for 'DateRange' values.
--
-- >>> mkDateRange @(Either Text) (read "2021-01-01") (read "2021-12-31")
-- Right (MkDateRange {dateRangeSince = 2021-01-01, dateRangeUntil = 2021-12-31})
-- >>> mkDateRange @(Either Text) (read "2021-01-01") (read "2021-01-01")
-- Right (MkDateRange {dateRangeSince = 2021-01-01, dateRangeUntil = 2021-01-01})
-- >>> mkDateRange @(Either Text) (read "2021-01-02") (read "2021-01-01")
-- Left "Invalid date range (since > until): 2021-01-02 > 2021-01-01"
mkDateRange
  :: MonadError Text m
  => Date.Time.Day
  -> Date.Time.Day
  -> m DateRange
mkDateRange s u = do
  when (s > u) (throwError ("Invalid date range (since > until): " <> tshow s <> " > " <> tshow u))
  pure (MkDateRange s u)
