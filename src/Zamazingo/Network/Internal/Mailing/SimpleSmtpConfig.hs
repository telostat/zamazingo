-- | This is an internal module that provides a simple SMTP server configuration
-- for SMTP clients.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Zamazingo.Network.Internal.Mailing.SimpleSmtpConfig where

import qualified Deriving.Aeson.Stock            as DAS
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
  deriving (Eq, DAS.Generic, Ord, Show)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "simpleSmtpConfig" SimpleSmtpConfig
