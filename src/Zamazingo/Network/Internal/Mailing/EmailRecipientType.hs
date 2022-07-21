-- | This is an internal module providing e-mail recipient types.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Zamazingo.Network.Internal.Mailing.EmailRecipientType where

import qualified Data.Aeson      as Aeson
import           GHC.Generics    (Generic)
import           Zamazingo.Aeson (commonAesonOptions)


-- | Type encoding for email recipient values (to, cc or bcc)
data EmailRecipientType =
    EmailRecipientTypeTo
  | EmailRecipientTypeCc
  | EmailRecipientTypeBcc
  deriving (Eq, Generic, Ord, Show)


instance Aeson.FromJSON EmailRecipientType where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "EmailRecipientType"


instance Aeson.ToJSON EmailRecipientType where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "EmailRecipientType"

