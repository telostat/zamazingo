-- | This internal module provides mailess functionality.
--
-- See <https://github.com/telostat/mailess> to learn more about Mailess.

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Zamazingo.Network.Internal.Mailing.Mailess where

import           Control.Exception                                     (Exception)
import           Control.Monad.Catch                                   (MonadCatch(catch), MonadThrow(throwM))
import           Control.Monad.IO.Class                                (MonadIO(liftIO))
import qualified Data.Aeson                                            as Aeson
import qualified Data.ByteString                                       as B
import qualified Data.List.NonEmpty                                    as NE
import           Data.Text                                             (Text, unpack)
import           GHC.Generics                                          (Generic)
import           GHC.Stack                                             (HasCallStack, callStack, prettyCallStack)
import qualified Network.HTTP.Client.MultipartFormData                 as MP
import qualified Network.HTTP.Simple                                   as NS
import           Zamazingo.Aeson                                       (commonAesonOptions)
import           Zamazingo.Network.Internal.HttpUrl                    (HttpUrl)
import           Zamazingo.Network.Internal.Mailing.EmailAddress       (EmailAddress)
import           Zamazingo.Network.Internal.Mailing.EmailRecipientType (EmailRecipientType(..))
import           Zamazingo.Network.Internal.Mailing.SimpleSmtpConfig   (SimpleSmtpConfig)
import           Zamazingo.Text                                        (TextEncoder(encodeString), tshow)


-- * Configuration
-- $configuration


-- | Type encoding for remote mailess API configuration.
newtype MailessConfig = MailessConfig
  { mailessConfigBaseUrl :: HttpUrl
  }
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON MailessConfig where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "mailessConfig"


instance Aeson.ToJSON MailessConfig where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "mailessConfig"


-- * Sending Emails
-- $sendingEmails


-- | Type encoding for attachments as a 2-tuple of file name and file contents.
type EmailAttachment = (Text, B.ByteString)


-- | Type encoding for a non-empty list of email recipients as a 2-tuple of
-- email recipient type and email address.
type EmailRecipients = NE.NonEmpty (EmailRecipientType, EmailAddress)


-- | Sends an email over a remote Mailess service.
sendMailess
  :: HasCallStack
  => MonadIO m
  => MonadThrow m
  => MonadCatch m
  => Aeson.ToJSON a
  => MailessConfig     -- ^ Mailess configuration.
  -> SimpleSmtpConfig  -- ^ SMTP configuration
  -> B.ByteString      -- ^ TXT email template.
  -> B.ByteString      -- ^ MJML email template.
  -> EmailAddress      -- ^ Email address to send the email from.
  -> Text              -- ^ Subject
  -> EmailRecipients   -- ^ Recipients
  -> a                 -- ^ Context
  -> [EmailAttachment] -- ^ List of attachments.
  -> m ()
sendMailess config smtp txtTmpl mjmTmpl from subject recipients context attachments =
  let
    metadata = MailessMetadata
      { mailessMetadataSmtp    = smtp
      , mailessMetadataFrom    = from
      , mailessMetadataTo      = fmap snd (NE.filter (\(x, _) -> x == EmailRecipientTypeTo) recipients)
      , mailessMetadataCc      = fmap snd (NE.filter (\(x, _) -> x == EmailRecipientTypeCc) recipients)
      , mailessMetadataBcc     = fmap snd (NE.filter (\(x, _) -> x == EmailRecipientTypeBcc) recipients)
      , mailessMetadataSubject = subject
      , mailessMetadataContext = context
      }
  in
    sendMailessUnsafe config txtTmpl mjmTmpl metadata attachments


-- | Prepares the base Mailess API request with the given desired Mailess API
-- endpoint path.
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> let config = MailessConfig $$(Z.decodeTextTH "https://mailess.localhost/")
-- >>> buildBaseMailessRequest "/sendmail" config
-- Request {
--   host                 = "mailess.localhost"
--   port                 = 443
--   secure               = True
--   requestHeaders       = []
--   path                 = "/sendmail"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- ...
buildBaseMailessRequest
  :: HasCallStack
  => MonadThrow m
  => MonadCatch m
  => Text          -- ^ Path to the Mailess endpoint
  -> MailessConfig -- ^ Mailess configuration.
  -> m NS.Request
buildBaseMailessRequest path MailessConfig{..} =
  NS.parseRequest (encodeString mailessConfigBaseUrl  <> unpack path) `catch` \(x :: NS.HttpException) ->
    throwM (MailessException ("Error while building the mailess request: " <> tshow x))


-- * Exceptions
-- $exceptions


-- | Exception type that can be thrown by mailess procedures.
data MailessException where
  MailessException :: HasCallStack => Text -> MailessException


instance Exception MailessException


instance Show MailessException where
  show (MailessException merr) = "MailessException! Underlying error was:\n " <> unpack merr <> "\nStack Trace is:\n" <> prettyCallStack callStack


-- * Unsafe
-- $unsafe


-- | Type encoding of the mailess metadata information as expected by the
-- Mailess API.
data MailessMetadata a = MailessMetadata
  { mailessMetadataSmtp    :: !SimpleSmtpConfig
  , mailessMetadataFrom    :: !EmailAddress
  , mailessMetadataTo      :: ![EmailAddress]
  , mailessMetadataCc      :: ![EmailAddress]
  , mailessMetadataBcc     :: ![EmailAddress]
  , mailessMetadataSubject :: !Text
  , mailessMetadataContext :: !a
  }
  deriving (Eq, Generic, Show)


instance (Aeson.FromJSON a) => Aeson.FromJSON (MailessMetadata a) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "mailessMetadata"


instance (Aeson.ToJSON a) => Aeson.ToJSON (MailessMetadata a) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "mailessMetadata"


-- | Low-level function to send emails over Mailess.
--
-- This is unsafe as the metadata may not have any recipients at all. Use 'sendMailess' instead.
sendMailessUnsafe
  :: HasCallStack
  => MonadIO m
  => MonadThrow m
  => MonadCatch m
  => Aeson.ToJSON a
  => MailessConfig     -- ^ Mailess configuration.
  -> B.ByteString      -- ^ TXT email template.
  -> B.ByteString      -- ^ MJML email template.
  -> MailessMetadata a -- ^ Mailess metadata.
  -> [EmailAttachment] -- ^ List of attachments.
  -> m ()
sendMailessUnsafe config tmplTxt tmplMjm metadata attachments = do
  request <- buildBaseMailessRequest "/sendmail" config
  let parts =
        [ (MP.partLBS "metadata" (Aeson.encode metadata)) {MP.partFilename = Just "_metadata.json"}
        , (MP.partBS "templatetxt" tmplTxt) {MP.partFilename = Just "_template.txt.hbs"}
        , (MP.partBS "templatemjm" tmplMjm) {MP.partFilename = Just "_template.mjm.hbs"}
        ] <> fmap (\(n, c) -> (MP.partBS "attachments" c) {MP.partFilename = Just (unpack n)} ) attachments
  formData <- MP.formDataBody parts request
  response <- liftIO (NS.httpLbs formData) `catch` \(x :: NS.HttpException) ->
    throwM (MailessException ("Error while sending the mailess request: " <> tshow x))
  case NS.getResponseStatusCode response of
    200 -> pure ()
    x -> throwM (MailessException ("Error during mailess request. Status Code:" <> tshow x <> ". Error Message: " <> tshow (NS.getResponseBody response)))
