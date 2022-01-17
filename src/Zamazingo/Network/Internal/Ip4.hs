-- | Internal module for network IPv4 address definition and related
-- functionality.

module Zamazingo.Network.Internal.Ip4 where

import           Control.Monad.Except              (MonadError(throwError))
import qualified Data.Aeson                        as Aeson
import           Data.Text                         (Text, pack, unpack)
import           Data.Void                         (Void)
import           Data.Word                         (Word8)
import qualified Language.Haskell.TH.Syntax        as TH.Syntax
import qualified Text.Megaparsec                   as MP
import qualified Text.Megaparsec.Char              as MPC
import           Text.Printf                       (printf)
import           Text.Read                         (readMaybe)
import           Zamazingo.Text.Internal.TextCodec
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding of IPv4 address values.
--
-- >>> minBound :: Ip4
-- 0.0.0.0
-- >>> maxBound :: Ip4
-- 255.255.255.255
newtype Ip4 = Ip4
  { ip4blocks :: (Word8, Word8, Word8, Word8)
  }
  deriving (Bounded, Eq, Ord, TH.Syntax.Lift)


-- | 'Show' instance for 'Ip4'.
--
-- >>> minBound :: Ip4
-- 0.0.0.0
-- >>> maxBound :: Ip4
-- 255.255.255.255
instance Show Ip4 where
  show = unpack . encodeText


instance TextCodec Ip4


instance TextDecoder Ip4 where
  decodeText x = maybe (throwError ("Not a valid IPv4 address: " <> x)) pure (MP.parseMaybe ip4Parser x)


instance TextEncoder Ip4 where
  encodeText (Ip4 (a, b, c, d)) = pack (printf "%d.%d.%d.%d" a b c d)


-- | 'Aeson.FromJSON' instance for 'Ip4'.
--
-- >>> Aeson.eitherDecode "\"0.0.0.0\"" :: Either String Ip4
-- Right 0.0.0.0
-- >>> Aeson.eitherDecode "\"255.255.255.255\"" :: Either String Ip4
-- Right 255.255.255.255
-- >>> Aeson.eitherDecode "\"255.255.255.256\"" :: Either String Ip4
-- Left "Error in $: Not a valid IPv4 address: 255.255.255.256"
instance Aeson.FromJSON Ip4 where
  parseJSON = jsonDecoderFromTextDecoder "Ip4"


-- | 'Aeson.ToJSON' instance for 'Ip4'.
--
-- >>> Aeson.encode (minBound :: Ip4)
-- "\"0.0.0.0\""
-- >>> Aeson.encode (maxBound :: Ip4)
-- "\"255.255.255.255\""
-- >>> Aeson.eitherDecode (Aeson.encode (minBound :: Ip4)) :: Either String Ip4
-- Right 0.0.0.0
-- >>> Aeson.eitherDecode (Aeson.encode (maxBound :: Ip4)) :: Either String Ip4
-- Right 255.255.255.255
instance Aeson.ToJSON Ip4 where
  toJSON = jsonEncoderFromTextEncoder


-- | 'Ip4' parser.
ip4Parser :: MP.Parsec Void Text Ip4
ip4Parser = fmap Ip4 $ (,,,)
  <$> word8Parser <* MPC.char '.'
  <*> word8Parser <* MPC.char '.'
  <*> word8Parser <* MPC.char '.'
  <*> word8Parser <* MP.eof


-- | Byte ('Word8') parser.
--
-- >>> MP.parseMaybe word8Parser "0"
-- Just 0
-- >>> MP.parseMaybe word8Parser "255"
-- Just 255
-- >>> MP.parseMaybe word8Parser "256"
-- Nothing
word8Parser :: MP.Parsec Void Text Word8
word8Parser = do
    mi <- maybe (fail "Can not read Word8 from textual value.") pure . readMaybe =<< MP.some MPC.digitChar
    if mi < (256 :: Integer)
      then pure (fromIntegral mi)
      else fail "Can not read Word8 from textual value: Maximum value for Word8 is 255"
