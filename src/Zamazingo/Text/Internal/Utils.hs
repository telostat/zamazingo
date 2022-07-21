-- | Internal module of utility definitions for working with textual data.

module Zamazingo.Text.Internal.Utils where

import Data.Profunctor (dimap)
import Data.Text       (Text, pack, unwords, words)
import Prelude         hiding (unwords, words)


-- | Pseudo-show method to produce 'Text' values from showable values.
--
-- >>> tshow (1 :: Int)
-- "1"
-- >>> :type tshow (1 :: Int)
-- tshow (1 :: Int) :: Text
tshow :: Show a => a -> Text
tshow = pack . show


-- | Removes leading and trailing whitespaces, replaces consecutive whitespaces
-- with a single space.
--
-- >>> sanitizeText ""
-- ""
-- >>> sanitizeText " "
-- ""
-- >>> sanitizeText "a "
-- "a"
-- >>> sanitizeText " a"
-- "a"
-- >>> sanitizeText " a "
-- "a"
-- >>> sanitizeText " a b "
-- "a b"
-- >>> sanitizeText "  a   b  "
-- "a b"
-- >>> sanitizeText " \r\n\ta \r\n\tb \r\n\t"
-- "a b"
sanitizeText :: Text -> Text
sanitizeText = withWords id


-- | Applies a textual transformation by words.
--
-- >>> withWords id ""
-- ""
-- >>> withWords id " "
-- ""
-- >>> withWords id " a  b "
-- "a b"
withWords :: ([Text] -> [Text]) -> Text -> Text
withWords = dimap words unwords
