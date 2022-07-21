-- | This is an internal module providing e-mail recipient types.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Zamazingo.Network.Internal.Mailing.EmailRecipientType where

import qualified Deriving.Aeson as DA


-- | Type encoding for email recipient values (to, cc or bcc)
data EmailRecipientType =
    EmailRecipientTypeTo
  | EmailRecipientTypeCc
  | EmailRecipientTypeBcc
  deriving (Eq, DA.Generic, Ord, Show)
  deriving (DA.FromJSON, DA.ToJSON) via DA.CustomJSON '[DA.ConstructorTagModifier (DA.StripPrefix "EmailRecipientType", DA.CamelToSnake)] EmailRecipientType
