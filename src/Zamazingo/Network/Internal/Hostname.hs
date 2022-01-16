-- | Internal module for network hostname definition and related functionality.

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Zamazingo.Network.Internal.Hostname where

import           Control.Monad.Except              (MonadError(throwError))
import qualified Data.Aeson                        as Aeson
import qualified Data.List
import           Data.Text                         (Text, intercalate, pack, unpack)
import           Data.Void                         (Void)
import qualified Language.Haskell.TH.Syntax        as TH
import qualified Text.Megaparsec                   as MP
import qualified Text.Megaparsec.Char              as MPC
import           Zamazingo.Text.Internal.TextCodec
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding of hostname values.
--
-- See https://en.wikipedia.org/wiki/Hostname
--
--
-- 'Hostname' is simply a newtype around 'Text'. The newtype constructor
-- 'MkHostname' is not a member of the public API:
--
-- 1. 'Hostname' has a 'TextCodec' instance. Therefore, the 'decodeText' method
--    of 'TextDecoder' acts like a smart constructor. This is the only way to
--    construct 'Hostname' values if users stick to the public API.
-- 2. Similarly, when the call-site needs the underlying 'Text' representation,
--    the 'encodeText' method of 'TextEncoder' can be used.
--
-- >>> localhost
-- "localhost"
-- >>> :type localhost
-- localhost :: Hostname
-- >>> $$(hostnameTH "localhost")
-- "localhost"
-- >>> $$(hostnameTH "zamazingo.localhost")
-- "zamazingo.localhost"
newtype Hostname = MkHostname Text
  deriving (Eq, Ord, TH.Lift)


-- | 'Show' instance for 'Hostname'.
--
-- >>> localhost
-- "localhost"
instance Show Hostname where
  show (MkHostname x) = show x


instance TextCodec Hostname


instance TextDecoder Hostname where
  decodeText x = maybe (throwError ("Not a valid hostname: " <> x)) pure (MP.parseMaybe hostnameParser x)


instance TextEncoder Hostname where
  encodeText (MkHostname x) = x


-- | 'Aeson.FromJSON' instance for 'Hostname'.
--
-- >>> Aeson.eitherDecode "\"localhost\"" :: Either String Hostname
-- Right "localhost"
-- >>> Aeson.eitherDecode "\"local.host\"" :: Either String Hostname
-- Right "local.host"
-- >>> Aeson.eitherDecode "\"local-host\"" :: Either String Hostname
-- Right "local-host"
-- >>> Aeson.eitherDecode "\"-localhost\"" :: Either String Hostname
-- Left "Error in $: Not a valid hostname: -localhost"
instance Aeson.FromJSON Hostname where
  parseJSON = jsonDecoderFromTextDecoder "Hostname"


-- | 'Aeson.ToJSON' instance for 'Hostname'.
--
-- >>> Aeson.encode localhost
-- "\"localhost\""
instance Aeson.ToJSON Hostname where
  toJSON = jsonEncoderFromTextEncoder


-- | 'Hostname' for localhost.
--
-- >>> localhost
-- "localhost"
localhost :: Hostname
localhost = MkHostname "localhost"


-- | Parser for hostname values.
--
-- A hostname is a non-empty collection of hostname labels separated by @.@ signs.
--
-- >>> MP.parseMaybe hostnameParser "localhost"
-- Just "localhost"
-- >>> MP.parseMaybe hostnameParser "local.host"
-- Just "local.host"
-- >>> MP.parseMaybe hostnameParser "0.0.0.0"
-- Just "0.0.0.0"
--
-- **TODO:** Is an IP address a valid hostname?
hostnameParser :: MP.Parsec Void Text Hostname
hostnameParser = do
  labels <- MP.sepBy1 hostnameLabelParser (MPC.char '.') <* MP.eof
  case labels of
    [] -> fail "Can not construct hostname from empty textual value"
    x  -> pure (MkHostname (intercalate "." x))


-- | Parser for hostname labels.
--
-- A hostname label starts with one of @[a-zA-Z0-9]@ and the rest may be a
-- series of same characters plus a @-@ sign. @-@ can not be at the end of the
-- label.
--
-- >>> MP.parseMaybe hostnameLabelParser ""
-- Nothing
-- >>> MP.parseMaybe hostnameLabelParser "localhost"
-- Just "localhost"
-- >>> MP.parseMaybe hostnameLabelParser "local-host"
-- Just "local-host"
-- >>> MP.parseMaybe hostnameLabelParser "-localhost"
-- Nothing
-- >>> MP.parseMaybe hostnameLabelParser "localhost-"
-- Nothing
-- >>> MP.parseMaybe hostnameLabelParser "local.host"
-- Nothing
hostnameLabelParser :: MP.Parsec Void Text Text
hostnameLabelParser = pack . Data.List.intercalate "-" <$> MP.sepBy1 (MP.some . MP.oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']) (MPC.char '-')


-- | Constructs a 'Hostname' value with compile-time checking using Template Haskell.
--
-- >>> $$(hostnameTH "zamazingo")
-- "zamazingo"
-- >>> $$(hostnameTH "-zamazingo")
-- ...
-- ...Not a valid hostname: -zamazingo
-- ...
hostnameTH :: Text -> TH.Q (TH.TExp Hostname)
hostnameTH = either (fail . unpack) (fmap TH.TExp . TH.lift) . (decodeText :: Text -> Either Text Hostname)
