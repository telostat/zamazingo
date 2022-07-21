{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- | Internal module for text decoding and encoding definitions.

module Zamazingo.Text.Internal.TextCodec where

import           Control.Monad.Except       (MonadError)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH.Syntax


-- | Typeclass that implies both 'TextDecoder' and 'TextEncoder' constraints.
class (TextDecoder a, TextEncoder a) => TextCodec a


instance TextCodec Text


instance TextCodec String


-- | Typeclass that allows decoding text into arbitrary types with error
-- propagation in case that the decoding fails.
--
-- >>> decodeText "zamazingo" :: Either Text Text
-- Right "zamazingo"
-- >>> decodeString "zamazingo" :: Either Text Text
-- Right "zamazingo"
-- >>> decodeBS "zamazingo" :: Either Text Text
-- Right "zamazingo"
-- >>> decodeLBS "zamazingo" :: Either Text Text
-- Right "zamazingo"
-- >>> decodeText "zamazinğo" :: Either Text Text
-- Right "zamazin\287o"
-- >>> decodeString "zamazinğo" :: Either Text Text
-- Right "zamazin\287o"
class TextDecoder a where
  decodeText :: MonadError Text m => Text -> m a

  decodeString :: MonadError Text m => String -> m a
  decodeString = decodeText . pack

  decodeBS :: MonadError Text m => B.ByteString -> m a
  decodeBS = decodeText . TE.decodeUtf8

  decodeLBS :: MonadError Text m => BL.ByteString -> m a
  decodeLBS = decodeText . TL.toStrict . TLE.decodeUtf8


instance TextDecoder Text where
  decodeText = pure


instance TextDecoder String where
  decodeText = pure . unpack


-- | Typeclass that allows encoding arbitrary types into text.
--
-- This typeclass does not give room for error propagation in case that the
-- encoding fails.
--
-- >>> encodeText "zamazingo"
-- "zamazingo"
-- >>> encodeString "zamazingo"
-- "zamazingo"
-- >>> encodeBS "zamazingo"
-- "zamazingo"
-- >>> encodeLBS "zamazingo"
-- "zamazingo"
-- >>> encodeText "zamazinğo"
-- "zamazin\287o"
-- >>> encodeString "zamazinğo"
-- "zamazin\287o"
class TextEncoder a where
  encodeText :: a -> Text

  encodeString :: a -> String
  encodeString = unpack . encodeText

  encodeBS :: a -> B.ByteString
  encodeBS = TE.encodeUtf8 . encodeText

  encodeLBS :: a -> BL.ByteString
  encodeLBS = TLE.encodeUtf8 . TL.fromStrict . encodeText


instance TextEncoder Text where
  encodeText = id


instance TextEncoder String where
  encodeText = pack


-- | Convenience function to create a 'Data.Aeson.Types.Parser' from an instance
-- of 'TextDecoder'.
jsonDecoderFromTextDecoder
  :: TextDecoder a
  => String                -- ^ Type name.
  -> Aeson.Value           -- ^ JSON value to parse @a@ from.
  -> Aeson.Types.Parser a  -- ^ JSON parser.
jsonDecoderFromTextDecoder s = Aeson.withText s (either (fail . unpack) pure . decodeText)


-- | Convenience function to create a "Data.Aeson" encoder from an instance of
-- 'TextEncoder'.
jsonEncoderFromTextEncoder
  :: TextEncoder a
  => a            -- ^ Value to encode as a JSON.
  -> Aeson.Value  -- ^ JSON value encoded from @a@.
jsonEncoderFromTextEncoder = Aeson.String . encodeText


-- | Constructs a decodable value with compile-time checking using Template Haskell.
--
-- This technique is borrowed from refined library.
--
-- >>> :set -XTemplateHaskell
-- >>> $$(decodeTextTH "zamazingo") :: String
-- "zamazingo"
-- >>> $$(decodeTextTH "zamazingo") :: Text
-- "zamazingo"
#if MIN_VERSION_template_haskell(2,17,0)
decodeTextTH
  :: forall a. (TextDecoder a, TH.Syntax.Lift a)
  => Text
  -> TH.Code TH.Q a
decodeTextTH = either (TH.Syntax.liftCode . fail . unpack) TH.Syntax.liftTyped . (decodeText :: Text -> Either Text a)
#else
decodeTextTH
  :: forall a. (TextDecoder a, TH.Syntax.Lift a)
  => Text
  -> TH.Q (TH.TExp a)
decodeTextTH = fmap TH.Syntax.TExp . either (fail . unpack) TH.Syntax.lift . (decodeText :: Text -> Either Text a)
#endif


-- | Type encoding of a pair of 'Text' values.
type TextPair = (Text, Text)


-- | Pairs given key and value as a 'TextPair'.
--
-- >>> pairText "key" "value"
-- ("key","value")
pairText
  :: TextEncoder k
  => TextEncoder v
  => k
  -> v
  -> TextPair
pairText k v = (encodeText k, encodeText v)


-- | Alias to 'pairText'.
(.=)
  :: TextEncoder k
  => TextEncoder v
  => k
  -> v
  -> TextPair
(.=) = pairText


-- | Pairs given key and value as a 'TextPair'.
--
-- >>> pairTextWithDefault "default" "key" (Nothing :: Maybe Text)
-- ("key","default")
-- >>> pairTextWithDefault "default" "key" (Just "value")
-- ("key","value")
pairTextWithDefault
  :: TextEncoder d
  => TextEncoder k
  => TextEncoder v
  => d
  -> k
  -> Maybe v
  -> TextPair
pairTextWithDefault d k v = (encodeText k, maybe (encodeText d) encodeText v)


-- | Alias to 'pairTextWithDefault'.
(.=?)
  :: TextEncoder d
  => TextEncoder k
  => TextEncoder v
  => d
  -> k
  -> Maybe v
  -> TextPair
(.=?) = pairTextWithDefault
