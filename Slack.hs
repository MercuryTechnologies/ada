{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Slack where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Header', JSON, Post, Required, Strict, (:>))

data AppsConnectionsOpenResponse = AppsConnectionsOpenResponse
    { ok :: Bool
    , url :: Text
    } deriving stock (Generic)
      deriving anyclass (FromJSON)

type AppsConnectionsOpen =
        "apps.connections.open"
    :>  Post '[JSON] AppsConnectionsOpenResponse

type API =
        Header' [Required, Strict]  "Authorization" Text
    :>  "api"
    :>  AppsConnectionsOpen
