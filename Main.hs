{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Codec.Serialise (Serialise)
import Control.Applicative (liftA2, many)
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, ParserPrefs(..))
import Servant.API ((:<|>)(..))
import Servant.Client (ClientM)

import OpenAI
    ( Choice(..)
    , CompletionRequest(..)
    , CompletionResponse(..)
    , Embedding(..)
    , EmbeddingRequest(..)
    , EmbeddingResponse(..)
    , Message(..)
    )

import qualified Codec.Serialise as Serialise
import qualified Control.Exception as Exception
import qualified Data.KdTree.Static as KdTree
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified Data.Vector.Split as Split
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI
import qualified Options.Applicative as Options
import qualified Servant.Client as Client

instance Semigroup a => Semigroup (ClientM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ClientM a) where
    mempty = pure mempty

data Mode
    = Index{ store :: FilePath, paths :: Vector FilePath }
    | Query{ store :: FilePath, query :: Text }

parsePath :: Parser FilePath
parsePath =
    Options.strArgument
        (   Options.metavar "FILE"
        <>  Options.action "file"
        )

parseIndexPath :: Parser FilePath
parseIndexPath =
    Options.strOption
        (   Options.long "store"
        <>  Options.metavar "FILE"
        <>  Options.action "file"
        )

parseIndex :: Parser Mode
parseIndex = do
    store <- parseIndexPath

    paths <- fmap Vector.fromList (many parsePath)

    return Index{..}

parseIndexInfo :: ParserInfo Mode
parseIndexInfo =
    Options.info
        parseIndex
        (Options.progDesc "Generate the index for the AI assistant")

parseQuery :: Parser Mode
parseQuery = do
    store <- parseIndexPath

    query <- Options.strArgument (Options.metavar "QUERY")

    pure Query{..}

parseQueryInfo :: ParserInfo Mode
parseQueryInfo =
    Options.info
        parseQuery
        (Options.progDesc "Ask the AI assistant a question")

data Options = Options
    { openAIAPIKey :: Text
    , mode :: Mode
    }

parseOptions :: Parser Options
parseOptions = do
    openAIAPIKey <- Options.strOption
        (   Options.long "openai-key"
        <>  Options.help "OpenAI API key"
        <>  Options.metavar "KEY"
        )

    mode <- Options.hsubparser
        (   Options.command "index" parseIndexInfo
        <>  Options.command "query" parseQueryInfo
        )

    return Options{..}

parseOptionsInfo :: ParserInfo Options
parseOptionsInfo =
    Options.info
        (Options.helper <*> parseOptions)
        (Options.progDesc "A helpful AI assistant for Mercury engineers")

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    result <- io

    case result of
        Left  clientError -> Exception.throwIO clientError
        Right x           -> return x

parserPrefs :: ParserPrefs
parserPrefs = Options.defaultPrefs
    { prefMultiSuffix = "..."
    , prefShowHelpOnError = True
    , prefHelpShowGlobal = True
    }

data IndexedContent = IndexedContent
    { content :: Text
    , embedding :: Vector Double
    } deriving stock (Generic)
      deriving anyclass (Serialise)

embeddingModel :: Text
embeddingModel = "text-embedding-3-large"

labeled :: Text -> Vector Text -> Text
labeled label entries =
    Text.intercalate "\n\n" (Vector.toList (Vector.imap renderEntry entries))
  where
    renderEntry :: Int -> Text -> Text
    renderEntry index entry = [__i|
        #{label} \##{index}:

        #{entry}
    |]

main :: IO ()
main = do
    Options{..} <- Options.customExecParser parserPrefs parseOptionsInfo

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 55_000_000
            }

    manager <- TLS.newTlsManagerWith managerSettings

    baseUrl <- Client.parseBaseUrl "https://api.openai.com"

    let clientEnv = Client.mkClientEnv manager baseUrl

    let (embeddings :<|> completions) = OpenAI.getClient header
          where
            header = "Bearer " <> openAIAPIKey

    let validateEmbeddingResponse data_ input = do
            unless (Vector.length data_ == Vector.length input) do
                fail [__i|
                    Internal error: the OpenAPI API returned the wrong number of embeddings

                    The OpenAPI API should return exactly as many embeddings as inputs that we
                    provided, but returned a different number of embeddings:

                    \# of inputs provided    : #{Vector.length input}
                    \# of embeddings returned: #{Vector.length data_}
                |]

    let clientM = case mode of
            Index{..} -> do
                inputs <- liftIO (Vector.mapM Text.IO.readFile paths)

                let chunkedInputs =
                        Vector.concatMap (Vector.fromList . Text.chunksOf 10000) inputs

                let index input = do
                        let embeddingRequest = EmbeddingRequest{..}
                              where
                                model = embeddingModel

                        EmbeddingResponse{..} <- embeddings embeddingRequest

                        liftIO (validateEmbeddingResponse data_ input)

                        return data_

                data_ <- foldMap index (Split.chunksOf 1097 chunkedInputs)

                let indexedContents = Vector.zipWith combine chunkedInputs data_
                      where
                        combine content Embedding{ embedding } =
                            IndexedContent{..}

                liftIO (Serialise.writeFileSerialise store indexedContents)
            Query{..} -> do
                indexedContents <- liftIO (Serialise.readFileDeserialise store)

                let kdTree =
                        KdTree.build (Vector.toList . Main.embedding)
                            (Vector.toList indexedContents)

                let input = Vector.singleton query

                let embeddingRequest = EmbeddingRequest{..}
                      where
                        model = embeddingModel

                EmbeddingResponse{..} <- embeddings embeddingRequest

                liftIO (validateEmbeddingResponse data_ input)

                let indexedContent = IndexedContent{..}
                      where
                        content = query
                        embedding = OpenAI.embedding (Vector.head data_)

                let neighbors = KdTree.kNearest kdTree 15 indexedContent

                let entries =
                        fmap Main.content (Vector.fromList neighbors)

                let completionRequest = CompletionRequest{..}
                      where
                        message = Message{..}
                          where
                            role = "user"

                        messages = [ message ]

                        max_tokens = Just 1024

                        model = "gpt-4-0125-preview"

                        content = [__i|
                            #{labeled "Context" entries}

                            Query:

                            #{query}
                        |]

                CompletionResponse{..} <- completions completionRequest

                let toContent :: Choice -> Text
                    toContent Choice{ message = Message{..} } = content

                liftIO case choices of
                    [ choice ] -> do
                        Text.IO.putStrLn (toContent choice)
                    _ -> do
                        Text.IO.putStr (labeled "Choice" (fmap toContent choices))

    result <- Client.runClientM clientM clientEnv

    case result of
        Left  clientError -> Exception.throwIO clientError
        Right x           -> return x
