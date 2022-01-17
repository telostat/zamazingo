-- | Internal module for non-empty text data definitions and related
-- functionality.

module Zamazingo.Text.Internal.NonEmptyText where

import           Control.Monad.Except              (MonadError(throwError))
import qualified Data.Aeson                        as Aeson
import           Data.Text                         (Text)
import qualified Language.Haskell.TH.Syntax        as TH
import           Zamazingo.Text.Internal.TextCodec
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding for non-empty 'Text' values.
--
-- 'NonEmptyText' is simply a newtype around 'Text'. The newtype constructor
-- 'MkNonEmptyText' is not a member of the public API:
--
-- 1. 'NonEmptyText' has a 'TextCodec' instance. Therefore, the 'decodeText'
--    method of 'TextDecoder' acts like a smart constructor. This is the only
--    way to construct 'NonEmptyText' values if users stick to the public API.
-- 2. Similarly, when the call-site needs the underlying 'Text' representation,
--    the 'encodeText' method of 'TextEncoder' can be used.
--
-- >>> let zamazingo = "zamazingo" :: Text
-- >>> decodeText zamazingo :: Either Text NonEmptyText
-- Right "zamazingo"
-- >>> fmap encodeText (decodeText zamazingo :: Either Text NonEmptyText) == Right zamazingo
-- True
--
-- There is a 'Semigroup' instance:
--
-- >>> zamazingo <> zamazingo
-- "zamazingozamazingo"
--
-- You can use the template-haskell constructor:
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> $$(Z.decodeTextTH "zamazingo") :: NonEmptyText
-- "zamazingo"
--
-- This will cause compile time error if the text is empty:
--
-- >>> $$(Z.decodeTextTH "") :: NonEmptyText
-- ...
-- ... Can not create non-empty text value with empty text parameter
-- ...
newtype NonEmptyText = MkNonEmptyText Text
  deriving (Eq, Ord, Semigroup, TH.Lift)


-- | 'Show' instance for 'NonEmptyText'.
--
-- >>> decodeText "hello" :: Either Text NonEmptyText
-- Right "hello"
instance Show NonEmptyText where
  show = show . encodeText


instance TextCodec NonEmptyText


instance TextDecoder NonEmptyText where
  decodeText "" = throwError "Can not create non-empty text value with empty text parameter"
  decodeText x  = pure (MkNonEmptyText x)


instance TextEncoder NonEmptyText where
  encodeText (MkNonEmptyText x) = x


-- | 'Aeson.FromJSON' instance for 'NonEmptyText'.
--
-- >>> Aeson.eitherDecode "\"zamazingo\"" :: Either String NonEmptyText
-- Right "zamazingo"
-- >>> Aeson.eitherDecode "\"\"" :: Either String NonEmptyText
-- Left "Error in $: Can not create non-empty text value with empty text parameter"
instance Aeson.FromJSON NonEmptyText where
  parseJSON = jsonDecoderFromTextDecoder "NonEmptyText"


-- | 'Aeson.ToJSON' instance for 'NonEmptyText'.
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> Aeson.encode ($$(Z.decodeTextTH "zamazingo") :: NonEmptyText)
-- "\"zamazingo\""
instance Aeson.ToJSON NonEmptyText where
  toJSON = jsonEncoderFromTextEncoder
