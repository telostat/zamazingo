-- | Internal module contains data definition and related functionality for
-- secrets that are nothing but a newtype around 'Text' value overriding the
-- 'Show' instance to avoid accidental leaking of such data.

module Zamazingo.Text.Internal.Secret where

import qualified Data.Aeson                        as Aeson
import           Data.Text                         (Text)
import qualified Language.Haskell.TH.Syntax        as TH.Syntax
import           Zamazingo.Text.Internal.TextCodec
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding for secret textual values.
--
-- This is nothing but a newtype around 'Text' value overriding the
-- 'Show' instance to avoid accidental leaking of such data.
--
-- >>> let zamazingo = "zamazingo" :: Text
-- >>> decodeText zamazingo :: Either Text Secret
-- Right <REDACTED>
-- >>> fmap encodeText (decodeText zamazingo :: Either Text Secret) == Right zamazingo
-- True
--
-- You can use the template-haskell constructor:
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> $$(Z.decodeTextTH "zamazingo") :: Secret
-- <REDACTED>
--
-- There is a 'Semigroup' instance:
--
-- >>> let hebele = $$(Z.decodeTextTH "hebele") :: Secret
-- >>> let hubele = $$(Z.decodeTextTH "hubele") :: Secret
-- >>> hebele <> hubele
-- <REDACTED>
-- >>> Z.encodeText (hebele <> hubele)
-- "hebelehubele"
newtype Secret = MkSecret Text
  deriving (Eq, Ord, Semigroup, TH.Syntax.Lift)


-- | 'Show' instance for 'Secret'.
--
-- >>> decodeText "secret" :: Either Text Secret
-- Right <REDACTED>
instance Show Secret where
  show = const "<REDACTED>"


instance TextCodec Secret


instance TextDecoder Secret where
  decodeText = pure . MkSecret


instance TextEncoder Secret where
  encodeText (MkSecret x) = x


-- | 'Aeson.FromJSON' instance for 'Secret'.
--
-- >>> Aeson.eitherDecode "\"\"" :: Either String Secret
-- Right <REDACTED>
-- >>> Aeson.eitherDecode "\"zamazingo\"" :: Either String Secret
-- Right <REDACTED>
instance Aeson.FromJSON Secret where
  parseJSON = jsonDecoderFromTextDecoder "Secret"


-- | 'Aeson.ToJSON' instance for 'Secret'.
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> Aeson.encode ($$(Z.decodeTextTH "zamazingo") :: Secret)
-- "\"zamazingo\""
instance Aeson.ToJSON Secret where
  toJSON = jsonEncoderFromTextEncoder
