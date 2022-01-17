-- | Convenience definitions for network related types and functions.

module Zamazingo.Network
  ( Host
  , Hostname
  , Ip4(..)
  , Port(unPort)
  , mkPort
  ) where

import Zamazingo.Network.Internal.Host     (Host)
import Zamazingo.Network.Internal.Hostname (Hostname)
import Zamazingo.Network.Internal.Ip4      (Ip4(..))
import Zamazingo.Network.Internal.Port     (Port(unPort), mkPort)
