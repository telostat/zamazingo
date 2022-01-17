-- | This module provides HTTP URI (Web URL, or informally, Web address) data
-- definition and related functionality.

{-# LANGUAGE QuasiQuotes #-}

module Zamazingo.Network.Internal.HttpUrl where

import           Control.Monad                     (when)
import           Control.Monad.Except              (MonadError(throwError))
import qualified Data.Aeson                        as Aeson
import           Data.Text                         (Text)
import qualified Language.Haskell.TH.Syntax        as TH.Syntax
import qualified Text.URI                          as Uri
import qualified Text.URI.QQ                       as Uri.QQ
import           Zamazingo.Text.Internal.TextCodec
                 ( TextCodec
                 , TextDecoder(..)
                 , TextEncoder(..)
                 , jsonDecoderFromTextDecoder
                 , jsonEncoderFromTextEncoder
                 )


-- | Type encoding of an HTTP URI.
--
-- >>> import Data.Text
-- >>> decodeText "http://localhost" :: Either Text HttpUrl
-- Right URI {uriScheme = Just "http", uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = "localhost", authPort = Nothing}), uriPath = Nothing, uriQuery = [], uriFragment = Nothing}
-- >>> decodeText "http:" :: Either Text HttpUrl
-- Left "Not a valid HTTP(S) URI in our concept: http:"
-- >>> decodeText "http:/" :: Either Text HttpUrl
-- Left "Not a valid HTTP(S) URI in our concept: http:/"
-- >>> decodeText "http://" :: Either Text HttpUrl
-- Left "Not a valid HTTP(S) URI in our concept: http://"
-- >>> decodeText "http:///" :: Either Text HttpUrl
-- Left "Not a valid HTTP(S) URI in our concept: http:///"
--
-- >>> let (Right url) = decodeText "https://localhost:8443/" :: Either Text HttpUrl
-- >>> encodeText url
-- "https://localhost:8443"
-- >>> let (Right url) = decodeText "https://localhost:443/" :: Either Text HttpUrl
-- >>> encodeText url
-- "https://localhost:443"
-- >>> let (Right url) = decodeText "https://localhost/" :: Either Text HttpUrl
-- >>> encodeText url
-- "https://localhost"
-- >>> let (Right url) = decodeText "https://localhost/hebele/hubele/" :: Either Text HttpUrl
-- >>> encodeText url
-- "https://localhost/hebele/hubele/"
newtype HttpUrl = MkHttpUrl
  { getHttpUri :: Uri.URI
  }
  deriving (Eq, Ord, TH.Syntax.Lift)


-- | 'Show' instance for 'HttpUrl'.
instance Show HttpUrl where
  show = show . getHttpUri


instance TextCodec HttpUrl


instance TextDecoder HttpUrl where
  decodeText t = case Uri.mkURI t of
    Left _  -> err
    Right u -> either (const err) pure (mkHttpUrl u)
    where
      err = throwError ("Not a valid HTTP(S) URI in our concept: " <> t)


instance TextEncoder HttpUrl where
  encodeText = Uri.render . getHttpUri


-- | 'Aeson.FromJSON' instance for 'HttpUrl'.
--
-- >>> Aeson.eitherDecode "\"https://local.host\"" :: Either String HttpUrl
-- Right URI {uriScheme = Just "https", uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = "local.host", authPort = Nothing}), uriPath = Nothing, uriQuery = [], uriFragment = Nothing}
-- >>> Aeson.eitherDecode "\"https://\"" :: Either String HttpUrl
-- Left "Error in $: Not a valid HTTP(S) URI in our concept: https://"
-- >>> Aeson.eitherDecode "\"smtp://localhost\"" :: Either String HttpUrl
-- Left "Error in $: Not a valid HTTP(S) URI in our concept: smtp://localhost"
instance Aeson.FromJSON HttpUrl where
  parseJSON = jsonDecoderFromTextDecoder "HttpUrl"


-- | 'Aeson.ToJSON' instance for 'HttpUrl'.
--
-- >>> :set -XTemplateHaskell
-- >>> import qualified Zamazingo as Z
-- >>> Aeson.encode ($$(Z.decodeTextTH "https://local.host") :: HttpUrl)
-- "\"https://local.host\""
instance Aeson.ToJSON HttpUrl where
  toJSON = jsonEncoderFromTextEncoder


-- | Smart constructor for 'HttpUrl' values.
mkHttpUrl :: MonadError Text m => Uri.URI -> m HttpUrl
mkHttpUrl u = case Uri.uriScheme u of
  Nothing -> err
  Just ss -> do
    when ((ss /= uriSchemeHttp && ss /= uriSchemeHttps) || either (const "") (Uri.unRText . Uri.authHost) (Uri.uriAuthority u) == "") err
    pure (MkHttpUrl u)
  where
    err = throwError ("URI is not an HTTP(S) URI: " <> Uri.render u)


-- | HTTP URI scheme.
uriSchemeHttp :: Uri.RText l
uriSchemeHttp = [Uri.QQ.scheme|http|]


-- | HTTPS URI scheme.
uriSchemeHttps :: Uri.RText l
uriSchemeHttps = [Uri.QQ.scheme|https|]
