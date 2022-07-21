-- | This module provides extra "Data.Aeson" and "Deriving.Aeson" definitions.

module Zamazingo.Aeson where

import qualified Data.Aeson as Aeson
import           Data.List  (stripPrefix)
import           Data.Maybe (fromMaybe)


-- | Common Aeson encoding/decoding options.
commonAesonOptions :: String -> Aeson.Options
commonAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.fieldLabelModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.constructorTagModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.sumEncoding = Aeson.TaggedObject
        { Aeson.tagFieldName = "type"
        , Aeson.contentsFieldName = "value"
        }
    }
