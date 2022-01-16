-- | Internal module for text decoding and encoding definitions.

module Zamazingo.Text.Internal.TextCodec where

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson.Types
import           Data.Text            (Text, pack, unpack)


-- | Typeclass that implies both 'TextDecoder' and 'TextEncoder' constraints.
class (TextDecoder a, TextEncoder a) => TextCodec a


instance TextCodec Text


instance TextCodec String


-- | Typeclass that allows decoding text into arbitrary types with error
-- propagation in case that the decoding fails.
class TextDecoder a where
  decodeText :: MonadError Text m => Text -> m a


instance TextDecoder Text where
  decodeText = pure


instance TextDecoder String where
  decodeText = pure . unpack


-- | Typeclass that allows encoding arbitrary types into text.
--
-- This typeclass does not give room for error propagation in case that the
-- encoding fails.
class TextEncoder a where
  encodeText :: a -> Text


instance TextEncoder Text where
  encodeText = id


instance TextEncoder String where
  encodeText = pack


-- | Convenience function to create a 'Data.Aeson.Types.Parser' from an instance
-- of 'TextDecoder'.
jsonDecoderFromTextDecoder
  :: TextDecoder a
  => String                -- ^ Type name.
  -> Aeson.Value           -- ^ JSON value to parse 'a' from.
  -> Aeson.Types.Parser a  -- ^ JSON parser.
jsonDecoderFromTextDecoder s = Aeson.withText s (either (fail . unpack) pure . decodeText)


-- | Convenience function to create a "Data.Aeson" encoder from an instance of
-- 'TextEncoder'.
jsonEncoderFromTextEncoder
  :: TextEncoder a
  => a            -- ^ Value to encode as a JSON.
  -> Aeson.Value  -- ^ JSON value encoded from 'a'.
jsonEncoderFromTextEncoder = Aeson.String . encodeText
