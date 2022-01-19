-- | This internal module provides email address data type definition and
-- related functionality.

module Zamazingo.Network.Internal.Mailing.EmailAddress where

import           Control.Monad              (unless)
import           Control.Monad.Except       (throwError)
import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import qualified Language.Haskell.TH.Syntax as TH.Syntax
import           Text.Email.Validate        (isValid)
import           Zamazingo.Text
                 ( TextCodec
                 , TextDecoder(decodeText)
                 , TextEncoder(encodeText)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding for email address type.
--
-- This is a newtype wrapper around 'Text' that is supposed to be a valid email
-- address. Use 'decodeText' to construct new email address values.
--
-- >>> decodeText @EmailAddress @(Either Text) "hebele@hubele.com"
-- Right (MkEmailAddress "hebele@hubele.com")
--
-- >>> decodeText @EmailAddress @(Either Text) "hebelehubele"
-- Left "Invalid e-mail address: hebelehubele"
newtype EmailAddress = MkEmailAddress Text
  deriving (Eq, Ord, Show, TH.Syntax.Lift)


instance TextCodec EmailAddress


instance TextDecoder EmailAddress where
  decodeText t = do
    unless (isValid (TE.encodeUtf8 t)) (throwError ("Invalid e-mail address: " <> t))
    pure (MkEmailAddress t)


instance TextEncoder EmailAddress where
  encodeText (MkEmailAddress x) = x


instance Aeson.FromJSON EmailAddress where
  parseJSON = jsonDecoderFromTextDecoder "EmailAddress"


instance Aeson.ToJSON EmailAddress where
  toJSON = jsonEncoderFromTextEncoder
