-- | Convenience definitions for network related types and functions.

module Zamazingo.Network
  ( Port(unPort)
  , mkPort
  , Hostname
  , Ip4(..)
  ) where

import Zamazingo.Network.Internal.Hostname (Hostname)
import Zamazingo.Network.Internal.Ip4      (Ip4(..))
import Zamazingo.Network.Internal.Port     (Port(unPort), mkPort)
