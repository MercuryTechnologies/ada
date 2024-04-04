{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Slack where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), Options(..), SumEncoding(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application, Request)

import Servant.API
    ( Header'
    , JSON
    , Post
    , ReqBody
    , Required
    , Strict
    , (:>)
    , (:<|>)
    )

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Base16.Types as Base16.Types
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai as Wai

fromJSONOptions :: Options
fromJSONOptions = Aeson.defaultOptions
    { constructorTagModifier = Aeson.camelTo2 '_'
    , sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , allNullaryToStringTag = False
    , tagSingleConstructors = True
    }

toJSONOptions :: Options
toJSONOptions = Aeson.defaultOptions
    { sumEncoding = UntaggedValue
    , allNullaryToStringTag = False
    , tagSingleConstructors = True
    }

data AppsConnectionsOpenResponse = AppsConnectionsOpenResponse
    { ok :: Bool
    , url :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type AppsConnectionsOpen =
        "apps.connections.open"
    :>  Post '[JSON] AppsConnectionsOpenResponse

data ChatPostMessageRequest = ChatPostMessageRequest
    { channel :: Text
    , thread_ts :: Maybe Text
    , text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data ChatPostMessageResponse = ChatPostMessageResponse
    { ok :: Bool
    , error :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type ChatPostMessage =
        "chat.postMessage"
    :>  ReqBody '[JSON] ChatPostMessageRequest
    :>  Post '[JSON] ChatPostMessageResponse

type Client =
        Header' [Required, Strict]  "Authorization" Text
    :>  "api"
    :>  (AppsConnectionsOpen :<|> ChatPostMessage)

data SocketEvent
    = Hello{ }
    | EventsAPI
        { envelope_id :: Text
        , payload :: Payload
        }
    | Disconnect
    deriving stock (Generic, Show)

instance FromJSON SocketEvent where
    parseJSON = Aeson.genericParseJSON fromJSONOptions

data Payload = Payload
    { event :: Event
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Event = Event
    { ts :: Text
    , channel :: Text
    , text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Acknowledgment = Acknowledgment
    { envelope_id :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data ServerRequest
    = URLVerification{ token :: Text, challenge :: Text }
    | EventCallback{ event :: Event }
    deriving stock (Generic, Show)

instance FromJSON ServerRequest where
    parseJSON = Aeson.genericParseJSON fromJSONOptions

data ServerResponse
    = ChallengeResponse{ challenge :: Text }
    | EmptyResponse
    deriving stock (Generic, Show)

instance ToJSON ServerResponse where
    toJSON EmptyResponse = Aeson.Object []
    toJSON value = Aeson.genericToJSON toJSONOptions value

    toEncoding EmptyResponse = toEncoding (Aeson.Object [])
    toEncoding value = Aeson.genericToEncoding toJSONOptions value

type Server =
        ReqBody '[JSON] ServerRequest
    :>  Post '[JSON] ServerResponse

-- https://api.slack.com/authentication/verifying-requests-from-slack
verify :: Text -> Request -> IO Bool
verify signingSecret request = do
    m <- MaybeT.runMaybeT do
        body <- liftIO (Wai.strictRequestBody request)

        Just timestampBytes <- return (lookup "X-Slack-Request-Timestamp" (Wai.requestHeaders request))

        Right timestampText <- return (Text.Encoding.decodeUtf8' timestampBytes)

        timestamp <- Time.parseTimeM True Time.defaultTimeLocale "%s" (Text.unpack timestampText)

        now <- liftIO (POSIX.getPOSIXTime)

        guard (abs (now - timestamp) <= 60 * 5)

        let baseBytes =
                ByteString.concat
                    [ "v0:"
                    , timestampBytes
                    , ":"
                    , ByteString.Lazy.toStrict body
                    ]

        let signingSecretBytes = Text.Encoding.encodeUtf8 signingSecret

        let hash = SHA256.hmac signingSecretBytes baseBytes

        let base16 = Base16.Types.extractBase16 (Base16.encodeBase16' hash)

        let signature = "v0=" <> base16

        Just xSlackSignature <- return (lookup "x-slack-signature" (Wai.requestHeaders request))

        guard (signature == xSlackSignature)

    return (Maybe.isJust m)

verificationMiddleware :: Text -> Application -> Application
verificationMiddleware signingSecret application request respond = do
    verified <- verify signingSecret request

    let response = Wai.responseBuilder HTTP.Types.status400 mempty mempty

    if verified
        then application request respond
        else respond response
