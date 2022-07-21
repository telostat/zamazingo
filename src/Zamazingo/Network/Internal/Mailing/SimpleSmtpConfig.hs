-- | This is an internal module that provides a simple SMTP server configuration
-- for SMTP clients.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Zamazingo.Network.Internal.Mailing.SimpleSmtpConfig where

import qualified Data.Aeson                      as Aeson
import           GHC.Generics                    (Generic)
import           Zamazingo.Aeson                 (commonAesonOptions)
import           Zamazingo.Network.Internal.Host (Host)
import           Zamazingo.Network.Internal.Port (Port)
import           Zamazingo.Text                  (Secret)


-- | Type encoding for simple SMTP server configuration for SMTP clients.
data SimpleSmtpConfig = SimpleSmtpConfig
  { simpleSmtpConfigHost     :: !Host
  , simpleSmtpConfigPort     :: !Port
  , simpleSmtpConfigUsername :: !(Maybe Secret)
  , simpleSmtpConfigPassword :: !(Maybe Secret)
  , simpleSmtpConfigSecure   :: !(Maybe Bool)
  }
  deriving (Eq, Generic, Ord, Show)


instance Aeson.FromJSON SimpleSmtpConfig where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "simpleSmtpConfig"


instance Aeson.ToJSON SimpleSmtpConfig where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "simpleSmtpConfig"
