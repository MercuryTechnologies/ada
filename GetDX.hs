{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module GetDX where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Header', JSON, Post, ReqBody, Required, Strict, (:>))

data EventsTrackRequest = EventsTrackRequest
    { timestamp :: Maybe Text
    , email :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data EventsTrackResponse = EventsTrackResponse{ }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

type API =
       Header' [Required, Strict] "Authorization" Text
    :> "events.track"
    :> ReqBody '[JSON] EventsTrackRequest
    :> Post '[JSON] EventsTrackResponse
