{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS -Wno-deprecations #-}

module Slack where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Aeson (FromJSON(..), Options(..), SumEncoding(..), ToJSON(..))
import Data.ByteString (ByteString)
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
import qualified Data.IORef as IORef
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
    , user :: Text
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
verify :: Text -> Request -> MaybeT IO ByteString
verify signingSecret request = do
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

    return (ByteString.Lazy.toStrict body)

verificationMiddleware :: Text -> Application -> Application
verificationMiddleware signingSecret application request respond = do
    verified <- MaybeT.runMaybeT (verify signingSecret request)

    case verified of
        Just originalRequestBody -> do
            ref <- IORef.newIORef (Just originalRequestBody)

            -- This is a hack to work around the fact that if a signing
            -- middleware consumes the request body then it's not available
            -- for the actual handler.  See:
            --
            -- https://github.com/haskell-servant/servant/issues/1120#issuecomment-1084318908
            let fakeRequestBody = do
                    m <- IORef.readIORef ref
                    case m of
                        Just bytes -> do
                            IORef.writeIORef ref Nothing

                            return bytes

                        Nothing -> do
                            return mempty

            let request' = request{ Wai.requestBody = fakeRequestBody }

            application request' respond

        Nothing -> do
            let response = Wai.responseBuilder HTTP.Types.status400 mempty mempty
            respond response
