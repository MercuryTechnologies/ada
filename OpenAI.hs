{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TypeOperators         #-}

module OpenAI where

import Data.Aeson (FromJSON(..), ToJSON, Options(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Servant.API
    (Header', JSON, Post, ReqBody, Required, Strict, (:>), (:<|>))

import qualified Data.Aeson as Aeson

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = dropTrailingUnderscore
    }

data EmbeddingRequest = EmbeddingRequest
    { input :: Vector Text
    , model :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data EmbeddingResponse = EmbeddingResponse
    { data_ :: Vector Embedding
    } deriving stock (Generic, Show)

instance FromJSON EmbeddingResponse where
    parseJSON = Aeson.genericParseJSON aesonOptions

data Embedding = Embedding
    { index :: Natural
    , embedding :: Vector Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type Embeddings =
        "embeddings"
    :>  ReqBody '[JSON] EmbeddingRequest
    :>  Post '[JSON] EmbeddingResponse

data CompletionRequest = CompletionRequest
    { messages :: Vector Message
    , model :: Text
    , max_tokens :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data Message = Message
    { content :: Text
    , role :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data CompletionResponse = CompletionResponse
    { choices :: Vector Choice
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Choice = Choice
    { message :: Message
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type Completions =
        "chat"
    :>  "completions"
    :>  ReqBody '[JSON] CompletionRequest
    :>  Post '[JSON] CompletionResponse

type API =
        Header' [Required, Strict]  "Authorization" Text
    :>  "v1"
    :>  (     Embeddings
        :<|>  Completions
        )
