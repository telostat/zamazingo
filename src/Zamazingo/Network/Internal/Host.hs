-- | Internal module for host definition and related functionality.

module Zamazingo.Network.Internal.Host where

import           Control.Monad.Except                (MonadError(catchError, throwError))
import qualified Data.Aeson                          as Aeson
import           Data.Text                           (unpack)
import qualified Language.Haskell.TH.Syntax          as TH.Syntax
import           Zamazingo.Network.Internal.Hostname (Hostname)
import           Zamazingo.Network.Internal.Ip4      (Ip4)
import           Zamazingo.Text
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding for hosts as a sum value of either of 'Ip4' or 'Hostname'.
data Host =
    HostByIpv4 !Ip4
  | HostByName !Hostname
  deriving (Eq, Ord, TH.Syntax.Lift)


instance Show Host where
  show = unpack . encodeText


-- | 'TextCodec' instance for 'Host'.
instance TextCodec Host


-- | 'TextDecoder' instance for 'Host'.
--
-- >>> import Data.Text
-- >>> decodeText "0.0.0.0" :: Either Text Host
-- Right 0.0.0.0
-- >>> decodeText "255.255.255.255" :: Either Text Host
-- Right 255.255.255.255
-- >>> decodeText "256.256.256.256" :: Either Text Host
-- Right 256.256.256.256
-- >>> decodeText "localhost" :: Either Text Host
-- Right localhost
-- >>> decodeText "-localhost" :: Either Text Host
-- Left "Not a valid IPv4 address or hostname: -localhost"
instance TextDecoder Host where
  decodeText t = tryIpv4 `catchError` const tryName `catchError` const err
    where
      tryIpv4 = fmap HostByIpv4 (decodeText t)
      tryName = fmap HostByName (decodeText t)
      err = throwError ("Not a valid IPv4 address or hostname: " <> t)


-- | 'TextEncoder' instance for 'Host'
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> encodeText ($$(Z.decodeTextTH "192.168.1.254") :: Host)
-- "192.168.1.254"
-- >>> encodeText ($$(Z.decodeTextTH "local.host") :: Host)
-- "local.host"
instance TextEncoder Host where
  encodeText x = case x of
    HostByIpv4 ip4  -> encodeText ip4
    HostByName host -> encodeText host


-- | 'Aeson.FromJSON' instance for 'Host'.
--
-- >>> Aeson.eitherDecode "\"192.168.1.254\"" :: Either String Host
-- Right 192.168.1.254
-- >>> Aeson.eitherDecode "\"local.host\"" :: Either String Host
-- Right local.host
-- >>> Aeson.eitherDecode "\"-local.host\"" :: Either String Host
-- Left "Error in $: Not a valid IPv4 address or hostname: -local.host"
instance Aeson.FromJSON Host where
  parseJSON = jsonDecoderFromTextDecoder "Host"


-- | 'Aeson.ToJSON' instance for 'Host'.
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> let ipv4 = $$(Z.decodeTextTH "192.168.1.254") :: Host
-- >>> let name = $$(Z.decodeTextTH "local.host") :: Host
-- >>> Aeson.encode ipv4
-- "\"192.168.1.254\""
-- >>> Aeson.encode name
-- "\"local.host\""
instance Aeson.ToJSON Host where
  toJSON = jsonEncoderFromTextEncoder
