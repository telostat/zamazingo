-- | Convenience for working with textual data.

module Zamazingo.Text
  ( sanitizeText
  , withWords
  , tshow
  , NonEmptyText
  , Secret(..)
  , module Zamazingo.Text.Internal.TextCodec
  ) where

import Zamazingo.Text.Internal.NonEmptyText (NonEmptyText)
import Zamazingo.Text.Internal.Secret       (Secret(..))
import Zamazingo.Text.Internal.TextCodec
import Zamazingo.Text.Internal.Utils        (sanitizeText, tshow, withWords)
