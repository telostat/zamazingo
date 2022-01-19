-- | This internal module provides utility definitions to work with date/time
-- values.

module Zamazingo.Time.Internal.Utils where

import           Data.Text (Text, pack, unpack)
import qualified Data.Time


-- | 'Data.Time.formatTime' working on 'Text' type.
--
-- >>> let date = Data.Time.fromGregorian 2021 12 31
-- >>> formatTime Data.Time.defaultTimeLocale "%d/%m/%Y" date
-- "31/12/2021"
formatTime :: Data.Time.FormatTime t => Data.Time.TimeLocale -> Text -> t -> Text
formatTime l f = pack . Data.Time.formatTime l (unpack f)


-- | 'formatTime' working on 'Text' type with default time locale.
--
-- >>> let date = Data.Time.fromGregorian 2021 12 31
-- >>> defaultFormatTime "%d/%m/%y" date
-- "31/12/21"
defaultFormatTime :: Data.Time.FormatTime t => Text -> t -> Text
defaultFormatTime f = pack . Data.Time.formatTime Data.Time.defaultTimeLocale (unpack f)
