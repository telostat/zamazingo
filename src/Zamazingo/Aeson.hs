-- | This module provides extra "Data.Aeson" and "Deriving.Aeson" definitions.

module Zamazingo.Aeson where

import qualified Data.Char      as C
import qualified Deriving.Aeson as DA


-- | Data definition for string modifier that lowers the first character of
-- identifier.
data LowerFirst


instance DA.StringModifier LowerFirst where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = C.toLower c : xs
