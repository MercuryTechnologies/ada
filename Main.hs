{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import Codec.Serialise (Serialise)
import Control.Applicative (liftA2, many, (<|>))
import Control.Exception.Safe (Exception, SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, ParserPrefs(..))
import Prelude hiding (error)
import Servant.API ((:<|>)(..))
import Servant.Client (ClientEnv, ClientM)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import Network.WebSockets.Client (ConnectionException(..))

import Slack
    ( Acknowledgment(..)
    , AppsConnectionsOpenResponse(..)
    , ChatPostMessageRequest(..)
    , ChatPostMessageResponse(..)
    , Event(..)
    , Payload(..)
    , ServerRequest(..)
    , ServerResponse(..)
    , SocketEvent(..)
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
import qualified Control.Logging as Logging
import qualified Data.Aeson as Aeson
import qualified Data.KdTree.Static as KdTree
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified Data.Vector.Split as Split
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.WebSockets.Client as WebSockets
import qualified OpenAI
import qualified Options.Applicative as Options
import qualified Servant.Client as Client
import qualified Servant.Server as Server
import qualified Slack

instance Semigroup a => Semigroup (ClientM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ClientM a) where
    mempty = pure mempty

data Mode
    = Index{ paths :: Vector FilePath }
    | Slack{ slackAPIKey :: Text, api :: SlackAPI }

data SlackAPI
    = EventAPI{ signingSecret :: Text, port :: Port, debug :: Bool }
    | SocketAPI{ slackSocketKey :: Text }

parsePath :: Parser FilePath
parsePath =
    Options.strArgument
        (   Options.metavar "FILE"
        <>  Options.action "file"
        )

parseIndex :: Parser Mode
parseIndex = do
    paths <- fmap Vector.fromList (many parsePath)

    return Index{..}

parseIndexInfo :: ParserInfo Mode
parseIndexInfo =
    Options.info
        parseIndex
        (Options.progDesc "Generate the index for the AI assistant")

parseEventAPI :: Parser SlackAPI
parseEventAPI = do
    port <- Options.option Options.auto
        (   Options.long "port"
        <>  Options.help "Server port to listen on"
        <>  Options.metavar "PORT"
        <>  Options.value 80
        )

    signingSecret <- Options.strOption
        (   Options.long "slack-signing-secret"
        <>  Options.help "Slack signing secret"
        <>  Options.metavar "KEY"
        )

    debug <- Options.switch
        (   Options.long "debug"
        <>  Options.help "Enable debug logging for incoming HTTP requests"
        )

    pure EventAPI{..}

parseSocketAPI :: Parser SlackAPI
parseSocketAPI = do
    slackSocketKey <- Options.strOption
        (   Options.long "slack-socket-key"
        <>  Options.help "Slack socket key"
        <>  Options.metavar "KEY"
        )

    pure SocketAPI{..}

parseSlack :: Parser Mode
parseSlack = do
    slackAPIKey <- Options.strOption
        (   Options.long "slack-api-key"
        <>  Options.help "Slack API key"
        <>  Options.metavar "KEY"
        )

    api <- parseSocketAPI <|> parseEventAPI

    pure Slack{..}

parseSlackInfo :: ParserInfo Mode
parseSlackInfo =
    Options.info
        parseSlack
        (Options.progDesc "Ask the AI assistant questions via Slack")

data Options = Options
    { openAIAPIKey :: Text
    , store :: FilePath
    , mode :: Mode
    }

parseOptions :: Parser Options
parseOptions = do
    openAIAPIKey <- Options.strOption
        (   Options.long "openai-key"
        <>  Options.help "OpenAI API key"
        <>  Options.metavar "KEY"
        )

    store <- Options.strOption
        (   Options.long "store"
        <>  Options.metavar "FILE"
        <>  Options.action "file"
        )

    mode <- Options.hsubparser
        (   Options.command "index" parseIndexInfo
        <>  Options.command "query" parseSlackInfo
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

retrying :: IO a -> IO a
retrying io = Exception.handle handler io
  where
    handler ConnectionClosed = retrying io
    handler CloseRequest{}   = retrying io
    handler exception        = Exception.throwIO exception

loggingExceptions :: IO a -> IO a
loggingExceptions io = Exception.handle handler io
  where
    handler (exception :: SomeException) = do
        Logging.warn (Text.pack (Exception.displayException exception))
        loggingExceptions io

data AdaException
    = MultipleChoices
    | PostFailure{ slackError :: Maybe Text }
    | ConnectionFailure
    | InvalidJSON{ bytes :: ByteString, jsonError :: Text }
    deriving stock (Show)

instance Exception AdaException where
    displayException MultipleChoices = [__i|
        Internal error: multiple choices

        The OpenAI sent back multiple responses when only one was expected
    |]

    displayException PostFailure{..} = [__i|
        Failed to post a chat message

        #{slackError}
    |]

    displayException ConnectionFailure = [__i|
        Failed to open a Slack Socket connection
    |]

    displayException InvalidJSON{..} = [__i|
        Internal error: Invalid JSON

        The Slack websocket sent a JSON message that failed to parse:

        Message: #{bytes}
        Error  : #{jsonError}
    |]

healthCheck :: Application -> Application
healthCheck application request respond
    | Wai.pathInfo request == [ "health" ] = do
        respond (Wai.responseBuilder HTTP.Types.status200 mempty mempty)
    | otherwise = do
        application request respond

main :: IO ()
main = Logging.withStderrLogging do
    Options{..} <- Options.customExecParser parserPrefs parseOptionsInfo

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 148_000_000
            }

    manager <- TLS.newTlsManagerWith managerSettings

    openAIEnv <- do
        baseUrl <- Client.parseBaseUrl "https://api.openai.com"

        return (Client.mkClientEnv manager baseUrl)

    let (embeddings :<|> completions) = Client.client @OpenAI.API Proxy header
          where
            header = "Bearer " <> openAIAPIKey

    let embed input = do
            let embeddingRequest = EmbeddingRequest{..}
                  where
                    model = embeddingModel

            EmbeddingResponse{..} <- embeddings embeddingRequest

            liftIO (validateEmbeddingResponse data_ input)

            let combine content Embedding{..} = IndexedContent{..}

            return (Vector.zipWith combine input data_)

    case mode of
        Index{..} -> do
            inputs <- Vector.mapM Text.IO.readFile paths

            let chunkedInputs =
                    Vector.concatMap (Vector.fromList . Text.chunksOf 10000) inputs

            indexedContents <- runClient openAIEnv (foldMap embed (Split.chunksOf 1097 chunkedInputs))

            Serialise.writeFileSerialise store indexedContents

        Slack{..} -> loggingExceptions do
            indexedContents <- Serialise.readFileDeserialise store

            let kdTree =
                    KdTree.build (Vector.toList . Main.embedding)
                        (Vector.toList indexedContents)

            slackEnv <- do
                baseUrl <- Client.parseBaseUrl "https://slack.com"

                return (Client.mkClientEnv manager baseUrl)

            let (_ :<|> chatPostMessage) = Client.client @Slack.Client Proxy header
                  where
                    header = "Bearer " <> slackAPIKey

            let respond Event{ text = query, ..}
                    -- Ada will receive webhooks for her own replies to direct
                    -- messages, so we ignore her own replies.  Otherwise, if
                    -- you DM Ada she'll keep replying to her own replies,
                    -- thinking they're messages another user has sent her.
                    | user == "U0509ATGR8X" = do
                        mempty

                    | otherwise = do
                        [ indexedContent ] <- runClient openAIEnv (embed [ query ])

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

                                    The following prompt contains a (non-exhaustive) Context of up to 15 relevant excerpts from our codebase that we've automatically gathered in hopes that they will help you respond, followed by a message containing the actual Slack message from one of our engineers.  The engineer is not privy to the Context, so if you mention entries in the Context as part of your answer they will not know what you're referring to unless you include any relevant excerpts from the context in your answer.

                                    #{labeled "Context" entries}

                                    Some other things to keep in mind as you answer:

                                    - Your Slack user ID is U0509ATGR8X, so if you see that in the Query that is essentially a user mentioning you (i.e. @Ada)

                                    - Try to avoid giving overly generic advice like "add more tests" or "coordinate with the team".  If you don't have something specific to say (perhaps because the context we're giving you doesn't have enough information) then it's okay to say that you don't have enough information to give a specific answer.

                                    - Please ensure that your response uses valid Slack markdown syntax (e.g. use *text* for bold instead of **text**).

                                    In particular, the Slack markdown is not GitHub-flavored markdown, so when you respond to the question please do not do something like this:

                                    ```haskell
                                    factorial :: Integer -> Integer
                                    factorial 0 = 1
                                    factorial n = n * factorial (n - 1)
                                    ```

                                    … because Slack markdown does not support the syntax specifier (e.g. "haskell", in the above example) like GitHub-flavored markdown does.  Instead, you want to always use plain code blocks (without any syntax-highlighting directive), like this:

                                    ```
                                    factorial :: Integer -> Integer
                                    factorial 0 = 1
                                    factorial n = n * factorial (n - 1)
                                    ```

                                    Message that you're replying to:

                                    #{query}
                                |]

                        CompletionResponse{..} <- runClient openAIEnv (completions completionRequest)

                        text <- case choices of
                            [ Choice{ message = Message{..} } ] -> return content
                            _                                   -> Exception.throwIO MultipleChoices

                        let chatPostMessageRequest =
                                ChatPostMessageRequest{ thread_ts = Just ts, .. }

                        ChatPostMessageResponse{..} <- runClient slackEnv (chatPostMessage chatPostMessageRequest)

                        unless ok (Exception.throwIO PostFailure{ slackError = error })

            let ready = Text.IO.putStrLn "Initialization complete"

            case api of
                EventAPI{..} -> do
                    ready

                    let server URLVerification{..} = do
                            pure ChallengeResponse{..}
                        server EventCallback{..} = liftIO do
                            _ <- Concurrent.forkIO (respond event)

                            return EmptyResponse{ }

                    let application =
                            healthCheck
                                (Slack.verificationMiddleware signingSecret
                                    (Server.serve @Slack.Server Proxy server)
                                )

                    let logging =
                            if debug
                            then RequestLogger.logStdoutDev
                            else RequestLogger.logStdout

                    Warp.run port (logging application)

                SocketAPI{..} -> do
                    retrying do
                        let (appsConnectionsOpen :<|> _) = Client.client @Slack.Client Proxy header
                              where
                                header = "Bearer " <> slackSocketKey

                        url <- runClient slackEnv do
                            AppsConnectionsOpenResponse{..} <- appsConnectionsOpen

                            liftIO (unless ok (Exception.throwIO ConnectionFailure))

                            return url

                        WebSockets.withConnection (Text.unpack url) \connection -> forever do
                            bytes <- WebSockets.receiveData connection

                            socketEvent <- case Aeson.eitherDecode bytes of
                                Left error ->
                                    Exception.throwIO InvalidJSON{ jsonError = Text.pack error, .. }

                                Right socketEvent ->
                                    return socketEvent

                            case socketEvent of
                                Hello{ } -> do
                                    ready

                                Disconnect{ } -> do
                                    Exception.throwIO ConnectionClosed

                                EventsAPI{..} -> do
                                    let Payload{..} = payload

                                    WebSockets.sendTextData connection (Aeson.encode Acknowledgment{..})

                                    _ <- Concurrent.forkIO (respond event)

                                    return ()
