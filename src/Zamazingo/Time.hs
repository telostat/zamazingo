-- | Convenience around "Data.Time" definitions.

module Zamazingo.Time
  ( -- * Convenience for 'IO'
    module Zamazingo.Time.Internal.IO

    -- * Utility Definitions
  , module Zamazingo.Time.Internal.Utils

    -- * DateRange
  , DateRange(dateRangeSince, dateRangeUntil)
  , mkDateRange

    -- * Time Zone Labels
  , TimeZoneLabel(unTimeZoneLabel)

    -- * Working with Aeson
    --
    -- | This section provides some guidance to the forgetful (eg. the original author
    -- of this section) on working with "Data.Aeson" and "Data.Time".
    --
    -- >>> import qualified Data.Aeson
    -- >>> import qualified Data.Time
    --
    -- >>> let date = Data.Time.fromGregorian 2021 12 31
    -- >>> Data.Aeson.encode date
    -- "\"2021-12-31\""
    -- >>> Data.Aeson.eitherDecode (Data.Aeson.encode date) :: Either String Data.Time.Day
    -- Right 2021-12-31
    --
    -- >>> let datetime = Data.Time.addUTCTime (-1) (Data.Time.UTCTime (Data.Time.fromGregorian 2022 1 1) 0)
    -- >>> Data.Aeson.encode datetime
    -- "\"2021-12-31T23:59:59Z\""
    -- >>> Data.Aeson.eitherDecode (Data.Aeson.encode datetime) :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC

    -- ** Precision
    --
    -- | "Data.Aeson" instances for "Data.Time" types parse date/time values
    -- with different precisions.
    --
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.9Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.9 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.999Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.999 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.999999Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.999999 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.999999999Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.999999999 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.999999999999Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.999999999999 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59.999999999999999Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59.999999999999 UTC

    -- ** Format Variations
    --
    -- | Date/Time format is flexible in terms of the @T@ separator between date
    -- and time values.
    --
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    --
    -- Offsets work fine, too:
    --
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59Z\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59+00\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59+00:00\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 23:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59+08\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 15:59:59 UTC
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59+08:00\"" :: Either String Data.Time.UTCTime
    -- Right 2021-12-31 15:59:59 UTC

    -- ** Without Timezone/Offset
    --
    -- | 'Data.Time.LocalTime' can be used in the lack of timezone/offset
    -- information.
    --
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31T23:59:59\"" :: Either String Data.Time.LocalTime
    -- Right 2021-12-31 23:59:59
    -- >>> Data.Aeson.eitherDecode "\"2021-12-31 23:59:59\"" :: Either String Data.Time.LocalTime
    -- Right 2021-12-31 23:59:59

    -- ** Time of Day
    --
    -- | 'Data.Time.TimeOfDay' has a convenient "Data.Aeson" instance.
    --
    -- >>> let tod = Data.Time.TimeOfDay 23 59 59
    -- >>> tod
    -- 23:59:59
    -- >>> Data.Aeson.encode tod
    -- "\"23:59:59\""
    -- >>> Data.Aeson.eitherDecode (Data.Aeson.encode tod) :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59
    --
    -- With various precision:
    --
    -- >>> Data.Aeson.eitherDecode "\"23\"" :: Either String Data.Time.TimeOfDay
    -- Left "Error in $: could not parse date: ':': not enough input"
    -- >>> Data.Aeson.eitherDecode "\"23:59\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:00
    -- >>> Data.Aeson.eitherDecode "\"23:59:59\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59
    -- >>> Data.Aeson.eitherDecode "\"23:59:59.999\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59.999
    -- >>> Data.Aeson.eitherDecode "\"23:59:59.999999\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59.999999
    -- >>> Data.Aeson.eitherDecode "\"23:59:59.999999999\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59.999999999
    -- >>> Data.Aeson.eitherDecode "\"23:59:59.999999999999\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59.999999999999
    -- >>> Data.Aeson.eitherDecode "\"23:59:59.999999999999999\"" :: Either String Data.Time.TimeOfDay
    -- Right 23:59:59.999999999999
  ) where

import Zamazingo.Time.Internal.DateRange     (DateRange(dateRangeSince, dateRangeUntil), mkDateRange)
import Zamazingo.Time.Internal.IO
import Zamazingo.Time.Internal.TimeZoneLabel (TimeZoneLabel(unTimeZoneLabel))
import Zamazingo.Time.Internal.Utils
