-- | Convenience definitions for network related types and functions.

module Zamazingo.Network
  ( Host
  , Hostname
  , HttpUrl(getHttpUri)
  , mkHttpUrl
  , Ip4(..)
  , Port(unPort)
  , mkPort
  , EmailAddress
  ) where

import Zamazingo.Network.Internal.Host                 (Host)
import Zamazingo.Network.Internal.Hostname             (Hostname)
import Zamazingo.Network.Internal.HttpUrl              (HttpUrl(getHttpUri), mkHttpUrl)
import Zamazingo.Network.Internal.Ip4                  (Ip4(..))
import Zamazingo.Network.Internal.Mailing.EmailAddress (EmailAddress)
import Zamazingo.Network.Internal.Port                 (Port(unPort), mkPort)
