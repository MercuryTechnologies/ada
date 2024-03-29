{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Slack where

import Data.Aeson (FromJSON(..), Options(..), SumEncoding(..), ToJSON)
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

type API =
        Header' [Required, Strict]  "Authorization" Text
    :>  "api"
    :>  (AppsConnectionsOpen :<|> ChatPostMessage)

data Event
    = Hello{ }
    | EventsAPI
        { envelope_id :: Text
        , payload :: Payload
        }
    deriving stock (Generic, Show)

instance FromJSON Event where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
        { constructorTagModifier = Aeson.camelTo2 '_'
        , sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
        , allNullaryToStringTag = False
        , tagSingleConstructors = True
        }

data Payload = Payload
    { event :: PayloadEvent
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data PayloadEvent = PayloadEvent
    { ts :: Text
    , channel :: Text
    , text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Acknowledgment = Acknowledgment
    { envelope_id :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)
