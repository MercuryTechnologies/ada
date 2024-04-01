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
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import Codec.Serialise (Serialise)
import Control.Applicative (liftA2, many)
import Control.Exception.Safe (Exception)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, ParserPrefs(..))
import Prelude hiding (error)
import Servant.API ((:<|>)(..))
import Servant.Client (ClientEnv, ClientM)
import Network.WebSockets.Client (ConnectionException(..))

import Slack
    ( Acknowledgment(..)
    , AppsConnectionsOpenResponse(..)
    , ChatPostMessageRequest(..)
    , ChatPostMessageResponse(..)
    , Event(..)
    , Payload(..)
    , PayloadEvent(..)
    )

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
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.KdTree.Static as KdTree
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified Data.Vector.Split as Split
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.WebSockets.Client as WebSockets
import qualified OpenAI
import qualified Options.Applicative as Options
import qualified Servant.Client as Client
import qualified Slack

instance Semigroup a => Semigroup (ClientM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ClientM a) where
    mempty = pure mempty

data Mode
    = Index{ store :: FilePath, paths :: Vector FilePath }
    | Query{ store :: FilePath }

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

    pure Query{..}

parseQueryInfo :: ParserInfo Mode
parseQueryInfo =
    Options.info
        parseQuery
        (Options.progDesc "Ask the AI assistant a question")

data Options = Options
    { openAIAPIKey :: Text
    , slackAPIKey :: Text
    , slackSocketKey :: Text
    , mode :: Mode
    }

parseOptions :: Parser Options
parseOptions = do
    openAIAPIKey <- Options.strOption
        (   Options.long "openai-key"
        <>  Options.help "OpenAI API key"
        <>  Options.metavar "KEY"
        )

    slackAPIKey <- Options.strOption
        (   Options.long "slack-api-key"
        <>  Options.help "Slack API key"
        <>  Options.metavar "KEY"
        )

    slackSocketKey <- Options.strOption
        (   Options.long "slack-socket-key"
        <>  Options.help "Slack socket key"
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

validateEmbeddingResponse :: Vector a -> Vector b -> IO ()
validateEmbeddingResponse data_ input = do
    unless (Vector.length data_ == Vector.length input) do
        fail [__i|
            Internal error: the OpenAPI API returned the wrong number of embeddings

            The OpenAPI API should return exactly as many embeddings as inputs that we
            provided, but returned a different number of embeddings:

            \# of inputs provided    : #{Vector.length input}
            \# of embeddings returned: #{Vector.length data_}
        |]

runClient :: ClientEnv -> ClientM a -> IO a
runClient env client = throws (Client.runClientM client env)

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    result <- io

    case result of
        Left  clientError -> Exception.throwIO clientError
        Right x           -> return x

retrying :: IO () -> IO ()
retrying io = Exception.handle handler io
  where
    handler ConnectionClosed = retrying io
    handler CloseRequest{} = retrying io
    handler _ = pure ()

main :: IO ()
main = do
    Options{..} <- Options.customExecParser parserPrefs parseOptionsInfo

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 55_000_000
            }

    manager <- TLS.newTlsManagerWith managerSettings

    let (embeddings :<|> completions) = Client.client (Proxy @OpenAI.API) header
          where
            header = "Bearer " <> openAIAPIKey

    let (appsConnectionsOpen :<|> _) = Client.client (Proxy @Slack.API) header
          where
            header = "Bearer " <> slackSocketKey

    let (_ :<|> chatPostMessage) = Client.client (Proxy @Slack.API) header
          where
            header = "Bearer " <> slackAPIKey

    openAIEnv <- do
        baseUrl <- Client.parseBaseUrl "https://api.openai.com"

        return (Client.mkClientEnv manager baseUrl)

    case mode of
        Index{..} -> do
            inputs <- Vector.mapM Text.IO.readFile paths

            let chunkedInputs =
                    Vector.concatMap (Vector.fromList . Text.chunksOf 10000) inputs

            let index input = do
                    let embeddingRequest = EmbeddingRequest{..}
                          where
                            model = embeddingModel

                    EmbeddingResponse{..} <- embeddings embeddingRequest

                    liftIO (validateEmbeddingResponse data_ input)

                    return data_

            data_ <- runClient openAIEnv (foldMap index (Split.chunksOf 1097 chunkedInputs))

            let indexedContents = Vector.zipWith combine chunkedInputs data_
                  where
                    combine content Embedding{ embedding } =
                        IndexedContent{..}

            Serialise.writeFileSerialise store indexedContents
        Query{..} -> do
            indexedContents <- Serialise.readFileDeserialise store

            let kdTree =
                    KdTree.build (Vector.toList . Main.embedding)
                        (Vector.toList indexedContents)

            slackEnv <- do
                baseUrl <- Client.parseBaseUrl "https://slack.com"

                return (Client.mkClientEnv manager baseUrl)

            retrying do
                url <- runClient slackEnv do
                    AppsConnectionsOpenResponse{..} <- appsConnectionsOpen

                    liftIO do
                        unless ok do
                            fail [__i|
                                Failed to open a Slack Socket connection
                            |]

                    return url

                WebSockets.withConnection (Text.unpack url) \connection -> do
                    forever do
                        bytes <- WebSockets.receiveData connection

                        event <- case Aeson.eitherDecode bytes of
                            Left e -> do
                                fail [__i|
                                    Internal error: Invalid JSON

                                    The Slack websocket sent a JSON message that failed to parse:

                                    Message: #{bytes}
                                    Error  : #{e}
                                |]

                            Right event -> do
                                return event

                        let acknowledge envelope_id =
                                WebSockets.sendTextData connection (Aeson.encode Acknowledgment{..})

                        case event of
                            Hello{ } -> do
                                Text.IO.putStrLn "Initialization complete"

                            Disconnect{ } -> do
                                Exception.throwIO ConnectionClosed

                            EventsAPI{..} -> do
                                let Payload{ event = event_ } = payload
                                let PayloadEvent{ text = query, ..} = event_

                                acknowledge envelope_id

                                _ <- Concurrent.forkIO do
                                    let input = Vector.singleton query

                                    let embeddingRequest = EmbeddingRequest{..}
                                          where
                                            model = embeddingModel

                                    EmbeddingResponse{..} <- runClient openAIEnv (embeddings embeddingRequest)

                                    validateEmbeddingResponse data_ input

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
                                                You are Ada, a helpful AI assistant whose persona is a foxgirl modeled after Senko from "The Helpful Fox Senko-san" (世話やきキツネの仙狐さん, Sewayaki Kitsune no Senko-san) and your avatar is a picture of Senko.  Your job is to respond to messages from Slack (such as the one at the end of this prompt) from engineers at Mercury (a startup that advertises itself as "Banking for ambitious companies") and your responses will be forwarded back to Slack as a reply to the original message (in a thread).

                                                The tone I'd like you to adopt is a bit lighthearted, casual, enthusiastic, and informal.

                                                Moreover, our company's core values are:

                                                - Think actively

                                                  Lead with curiosity.  Question, experiment, and find better ways to do things.

                                                - Be super helpful

                                                  Go above and beyond to solve problems, and do it as a team.

                                                - Act with humility

                                                  Treat everyone with respect and leave your ego at the door.

                                                - Appreciate quality

                                                  Pursue and recognize excellence to build something that lasts.

                                                - Focus on the outcome

                                                  Get the right results by taking extreme ownership of the process.

                                                - Seek wisdom

                                                  Be transparent.  Find connections in the universe's knowledge.  Use this information sensibly.

                                                … which may also be helpful to keep in mind as you answer the question.

                                                Some other things to keep in mind:

                                                - Your Slack user ID is U0509ATGR8X, so if you see that in the Query that is essentially a user mentioning you (i.e. @Ada)
                                                - Try to avoid giving overly generic advice like "add more tests" or "coordinate with the team".  If you don't have something specific to say (perhaps because the context we're giving you doesn't have enough information) then it's okay to say that you don't have enough information to give a specific answer.
                                                - Slack doesn't accept the "```${language}" prefix for syntax highlighting code blocks so just begin your code blocks with "```".

                                                The following prompt contains a (non-exhaustive) Context of up to 15 relevant excerpts from our codebase that we've automatically gathered in hopes that they will help you answer your question, followed by a message containing the actual question asked by one of our engineers.  The engineer is not privy to the Context, so if you mention entries in the Context as part of your answer they will not know what you're referring to unless you include any relevant excerpts from the context in your answer.

                                                #{labeled "Context" entries}

                                                Message that you're replying to:

                                                #{query}
                                            |]

                                    CompletionResponse{..} <- runClient openAIEnv (completions completionRequest)

                                    text <- case choices of
                                        [ Choice{ message = Message{..} } ] -> do
                                            return content
                                        _ -> do
                                            fail [__i|
                                                Internal error: multiple choices

                                                The OpenAI sent back multiple responses when only one was expected
                                            |]

                                    let chatPostMessageRequest =
                                            ChatPostMessageRequest{ thread_ts = Just ts, .. }

                                    ChatPostMessageResponse{..} <- runClient slackEnv (chatPostMessage chatPostMessageRequest)

                                    unless ok do
                                        fail [__i|
                                            Failed to post a chat message

                                            #{error}
                                        |]

                                return ()
