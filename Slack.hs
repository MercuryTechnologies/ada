{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeOperators         #-}

module Slack where

import Data.Aeson (FromJSON(..), Options(..), SumEncoding(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

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

import qualified Data.Aeson as Aeson

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
