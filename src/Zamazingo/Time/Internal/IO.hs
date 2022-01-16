-- | This internal module provides time related definitions that work inside
-- 'IO' but lifted from 'IO' into 'MonadIO' constraint.

module Zamazingo.Time.Internal.IO where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Time


-- * Inside 'MonadIO'
-- $insideMonadIO
--
-- Some of these functions are originally provided in "Data.Time" (under
-- different names) but they are redefined in this module lifted from 'IO' monad
-- into 'MonadIO' constraint.
--
-- >>> now <- getNow
-- >>> timezone <- getTimeZone
-- >>> nowLocal <- getNowLocal
--
-- >>> today <- getToday
-- >>> tomorrow <- getTomorrow
-- >>> yesterday <- getYesterday
-- >>> succ yesterday == today
-- True
-- >>> pred today == yesterday
-- True
-- >>> succ today == tomorrow
-- True
-- >>> pred tomorrow == today
-- True
-- >>> succ (succ yesterday) == tomorrow
-- True
-- >>> pred (pred tomorrow) == yesterday
-- True


-- | Returns the current 'Data.Time.UTCTime'.
getNow :: MonadIO m => m Data.Time.UTCTime
getNow = liftIO Data.Time.getCurrentTime


-- | Returns the current 'Data.Time.LocalTime' (as per current
-- 'Data.Time.TimeZone').
getNowLocal :: MonadIO m => m Data.Time.LocalTime
getNowLocal = do
  timezone <- getTimeZone
  Data.Time.utcToLocalTime timezone <$> getNow


-- | Returns the current 'Data.Time.TimeZone'.
getTimeZone :: MonadIO m => m Data.Time.TimeZone
getTimeZone = liftIO Data.Time.getCurrentTimeZone


-- | Returns the current 'Data.Time.Day'.
getToday :: MonadIO m => m Data.Time.Day
getToday = Data.Time.utctDay <$> liftIO Data.Time.getCurrentTime


-- | Returns the 'Data.Time.Day' of tomorrow as of now.
getTomorrow :: MonadIO m => m Data.Time.Day
getTomorrow = Data.Time.addDays 1 <$> getToday


-- | Returns the 'Data.Time.Day' of yesterday as of now.
getYesterday :: MonadIO m => m Data.Time.Day
getYesterday = Data.Time.addDays (-1) <$> getToday


-- | Returns the current 'Data.Time.DayOfWeek'.
getDayOfWeekToday :: MonadIO m => m Data.Time.DayOfWeek
getDayOfWeekToday = Data.Time.dayOfWeek <$> getToday


-- | Returns the 'Data.Time.DayOfWeek' of tomorrow as of now.
getDayOfWeekTomorrow :: MonadIO m => m Data.Time.DayOfWeek
getDayOfWeekTomorrow = Data.Time.dayOfWeek <$> getTomorrow


-- | Returns the 'Data.Time.DayOfWeek' of yesterday as of now.
getDayOfWeekYesterday :: MonadIO m => m Data.Time.DayOfWeek
getDayOfWeekYesterday = Data.Time.dayOfWeek <$> getYesterday

