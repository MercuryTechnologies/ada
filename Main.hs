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
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import Codec.Serialise (Serialise)
import Control.Applicative (liftA2, many, optional, (<|>))
import Control.Exception.Safe (Exception, SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import Data.Proxy (Proxy(..))
import GetDX (EventsTrackRequest(..), EventsTrackResponse(..))
import GHC.Generics (Generic)
import Network.HTTP.Types (Status(..))
import Options.Applicative (Parser, ParserInfo, ParserPrefs(..))
import Prelude hiding (error)
import Servant.API ((:<|>)(..))
import Servant.Client (ClientEnv, ClientError(..), ClientM, ResponseF(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import Network.WebSockets.Client (ConnectionException(..))

import OpenAI
    ( Choice(..)
    , CompletionRequest(..)
    , CompletionResponse(..)
    , Embedding(..)
    , EmbeddingRequest(..)
    , EmbeddingResponse(..)
    )
import Slack
    ( Acknowledgment(..)
    , AppsConnectionsOpenResponse(..)
    , ChatPostMessageRequest(..)
    , ChatPostMessageResponse(..)
    , ConversationsRepliesResponse(..)
    , Event(..)
    , Payload(..)
    , Profile(..)
    , ServerRequest(..)
    , ServerResponse(..)
    , SocketEvent(..)
    , User(..)
    , UsersInfoRequest(..)
    , UsersInfoResponse(..)
    )
import System.Console.Repline
    (CompleterStyle(..), ExitDecision(..), MultiLine(..), ReplOpts(..))

import qualified Codec.Serialise as Serialise
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Control.Logging as Logging
import qualified Data.Aeson as Aeson
import qualified Data.KdTree.Static as KdTree
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Clock.POSIX as Time.POSIX
import qualified Data.Vector as Vector
import qualified Data.Vector.Split as Split
import qualified GetDX
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
import qualified System.Console.Repline as Repline
import qualified System.Directory as Directory

instance Semigroup a => Semigroup (ClientM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ClientM a) where
    mempty = pure mempty

safeHead :: Vector a -> Maybe a
safeHead v = v !? 0

data Mode
    = Index{ paths :: Vector FilePath }
    | Slack{ slackAPIKey :: Text, api :: SlackAPI, getDXKey :: Maybe Text }
    | REPL

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

parseREPLInfo :: ParserInfo Mode
parseREPLInfo =
    Options.info
        (pure REPL)
        (Options.progDesc "Ask the AI assistant questions via a REPL")

parseSlack :: Parser Mode
parseSlack = do
    slackAPIKey <- Options.strOption
        (   Options.long "slack-api-key"
        <>  Options.help "Slack API key"
        <>  Options.metavar "KEY"
        )

    api <- parseSocketAPI <|> parseEventAPI

    getDXKey <- optional
        (Options.strOption
            (   Options.long "getdx-api-key"
            <>  Options.help "GetDX API key"
            <>  Options.metavar "KEY"
            )
        )

    pure Slack{..}

parseSlackInfo :: ParserInfo Mode
parseSlackInfo =
    Options.info
        parseSlack
        (Options.progDesc "Ask the AI assistant questions via Slack")

data Options = Options
    { openAIAPIKey :: Text
    , store :: FilePath
    , chatModel :: Text
    , embeddingModel :: Text
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
        <>  Options.help "The path to the index"
        <>  Options.metavar "FILE"
        <>  Options.action "file"
        )

    chatModel <- Options.strOption
        (   Options.long "chat-model"
        <>  Options.help "The model to use for answering questions (e.g. gpt-4o)"
        <>  Options.metavar "MODEL"
        )

    embeddingModel <- Options.strOption
        (   Options.long "embedding-model"
        <>  Options.help "The model to use for creating and querying the index (e.g. text-embedding-3-large)"
        <>  Options.metavar "MODEL"
        )

    mode <- Options.hsubparser
        (   Options.command "index" parseIndexInfo
        <>  Options.command "query" parseSlackInfo
        <>  Options.command "repl" parseREPLInfo
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
runClient env client = retry503 (throws (Client.runClientM client env))
  where
    retry503 io = Exception.handle handler io
      where
        handler
            (FailureResponse
                _
                Response{ responseStatusCode = Status{ statusCode = 503 } }
            ) =
            retry503 io
        handler e =
            Exception.throwIO e

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
    | PostFailure{ error :: Maybe Text }
    | ConnectionFailure
    | InvalidJSON{ bytes :: ByteString, jsonError :: Text }
    deriving stock (Show)

instance Exception AdaException where
    displayException MultipleChoices = [__i|
        Internal error: multiple choices

        The OpenAI API sent back multiple responses when only one was expected
    |]

    displayException PostFailure{..} = [__i|
        Failed to post a chat message

        #{error}
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

    let prepare = do
            indexedContents <- Serialise.readFileDeserialise store

            let kdTree =
                    KdTree.build (Vector.toList . Main.embedding)
                        (Vector.toList indexedContents)

            return \query maybeThreadMessages -> do
                [ indexedContent ] <- runClient openAIEnv (embed [ query ])

                let neighbors = KdTree.kNearest kdTree 15 indexedContent

                let contextTexts =
                        fmap Main.content (Vector.fromList neighbors)

                let history :: Text
                    history =
                        case maybeThreadMessages of
                            Nothing -> ""
                            Just threadMessages ->
                                let threadMessageTexts = do
                                        Slack.Message{..} <- threadMessages

                                        return [__i|#{user}: #{text}|]

                                in  [__i|
                                    The following messages precede the message you're replying to (in a thread):

                                    #{labeled "Thread Entry" threadMessageTexts}
                                    |]

                let completionRequest = CompletionRequest{..}
                      where
                        message = OpenAI.Message{..}
                          where
                            role = "user"

                        messages = [ message ]

                        max_tokens = Just 1024

                        model = chatModel

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

                            #{labeled "Context" contextTexts}

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

                            Similarly, you have to use *text* for bolding text and not **text**; again, this is because Slack markdown deviates from traditional markdown syntax.  For example, you don't want to do something like this:

                            1. **Some description**: Some text
                            2. **Another description**: Some more text

                            … because that will render wrong in Slack.  It will show up neither as bold nor italics; rather Slack will display the asterisks verbatim and it will make your response less readable.  Rather, you would instead do something like this:

                            1. *Some description*: Some text
                            1. *Another description*: Some more text

                            … which will correctly render "Some description" and "Another description" as bold text.

                            It's probably worth noting that for similar reasons you can't use *text* for italics in Slack markdown (because *text* is already reserved for bold text).  Instead you use _text_ if you want to italicize text.

                            Also, you want to avoid giving answers that are *too* long.  Like, sometimes a long answer is warranted (you are trying to be helpful after all!) and if it seems like the user is looking for a comprehensive answer then it can be worth writing out a full guide.  However, generally speaking there are a few things to keep in mind that should bias you towards a shorter answer:

                            - Users will be more likely to tag you in on shared public threads if you keep your answers shorter

                              The longer your answers are the more users will shy away from including you in conversations out of fear that you'll clobber the thread with a super long answer and make it less readable for everyone involved.

                            - Users will be able to parse out information of interest more easily if you keep your answers shorter

                            - You will respond more quickly to users if your answer is shorter

                              This is because your response is generated by OpenAI's API and the shorter your response the quicker the API can deliver the response to the user.

                            More generally, keeping your answers quicker and shorter help make your conversations with people more participatory.  Instead of talking "at" people and delivering a large monologue it's a more enjoyable experience for everyone involved if you are instead talking "with" people and the conversation is a gentle back and forth with nobody dominating the conversation.

                            #{history}

                            Finally, here is the actual message that you're replying to:

                            #{query}
                        |]

                CompletionResponse{..} <- runClient openAIEnv (completions completionRequest)

                case choices of
                    [ Choice{ message = OpenAI.Message{..} } ] ->
                        return content
                    _ ->
                        Exception.throwIO MultipleChoices

    case mode of
        Index{..} -> do
            let toInputs :: FilePath -> IO [Text]
                toInputs path = do
                    text <- Text.IO.readFile path

                    let chunkSize = 10000

                    let chunks = Text.chunksOf 10000 text

                    case chunks of
                        [ chunk ] -> do
                            return do
                                return [__i|
                                    Path: #{path}
                                    Contents:

                                    #{chunk}
                                |]
                        _ -> do
                            return do
                                (begin, chunk) <- zip [ 0, chunkSize.. ] chunks
                                let end = begin + Text.length chunk

                                return [__i|
                                    Path: #{path}
                                    Characters: #{begin}-#{end}
                                    Contents:

                                    #{chunk}
                                |]

            inputss <- mapM toInputs paths

            let inputs = Vector.fromList (concat inputss)

            exists <- Directory.doesFileExist store

            oldIndexedContents <- do
                if exists
                    then Serialise.readFileDeserialise store
                    else return []

            newIndexedContents <- runClient openAIEnv (foldMap embed (Split.chunksOf 1097 inputs))

            let indexedContents = oldIndexedContents <> newIndexedContents

            Serialise.writeFileSerialise store indexedContents

        REPL -> do
            ask <- prepare

            let banner SingleLine = pure "> "
                banner MultiLine  = pure "| "

            let command query = liftIO do
                    response <- ask (Text.pack query) Nothing

                    Text.IO.putStrLn response
                    Text.IO.putStrLn ""

            let options = mempty

            let prefix = Just ':'

            let multilineCommand = Just "paste"

            let tabComplete = Custom \(before, _after) -> pure (before, [])

            let initialiser = pure ()

            let finaliser = pure Exit

            Repline.evalReplOpts ReplOpts{..}

        Slack{..} -> loggingExceptions do
            slackEnv <- do
                baseUrl <- Client.parseBaseUrl "https://slack.com"

                return (Client.mkClientEnv manager baseUrl)

            let (_ :<|> chatPostMessage :<|> conversationsReplies :<|> usersInfo) = Client.client @Slack.Client Proxy header
                  where
                    header = "Bearer " <> slackAPIKey

            getDXEnv <- do
                baseUrl <- Client.parseBaseUrl "https://api.getdx.com"

                return (Client.mkClientEnv manager baseUrl)

            let reportGetDX request =
                    case getDXKey of
                        Nothing -> do
                            mempty

                        Just key -> runClient getDXEnv do
                            let header = "Bearer " <> key

                            let eventsTrack =
                                    Client.client @GetDX.API Proxy header

                            EventsTrackResponse{..} <- eventsTrack request

                            unless ok (Exception.throwIO PostFailure{..})

            ask <- prepare

            let respond Event{ text = query, ..}
                    -- Ada will receive webhooks for her own replies to direct
                    -- messages, so we ignore her own replies.  Otherwise, if
                    -- you DM Ada she'll keep replying to her own replies,
                    -- thinking they're messages another user has sent her.
                    | user == "U0509ATGR8X" = do
                        mempty

                    | otherwise = runClient slackEnv do
                        messages <- do
                            -- You can't directly use the `ts` from the event supplied by the webhook
                            -- because you supply that to the `conversations.replies` method then it will
                            -- only return replies *after* that message in the thread.  To obtain all of
                            -- the messages in the thread (including preceding ones), you need to fetch
                            -- the `thread_ts` from any message in the thread and use that.
                            --
                            -- Interestingly enough, the easiest way to get that `thread_ts` is also using
                            -- the same `conversation.replies` method, which is why we use that method
                            -- twice.
                            conversationRepliesResponse <- conversationsReplies channel ts (Just 1)

                            ts2 <- case conversationRepliesResponse of
                               ConversationsRepliesResponse{ ok = True, messages = Just (safeHead -> Just Slack.Message{ thread_ts = Just ts2 }) } -> return ts2
                               _ -> return ts

                            ConversationsRepliesResponse{..} <- conversationsReplies channel ts2 Nothing

                            return (if ok then messages else Nothing)

                        text <- liftIO (ask query messages)

                        do  let chatPostMessageRequest = ChatPostMessageRequest{ thread_ts = Just ts, .. }

                            ChatPostMessageResponse{..} <- chatPostMessage chatPostMessageRequest

                            unless ok (Exception.throwIO PostFailure{..})

                        Profile{..} <- do
                            usersInfoResponse <- usersInfo UsersInfoRequest{..}
                            case usersInfoResponse of
                                UsersInfoResponse{ user = Just userRecord, ok = True } -> do
                                    let User{..} = userRecord

                                    return profile

                                UsersInfoResponse{ error } -> do

                                    Exception.throwIO PostFailure{..}

                        do  let name = "Slack query"

                            posixTime <- liftIO (Time.POSIX.getPOSIXTime)

                            let timestamp = Text.pack (show (truncate posixTime :: Integer))

                            liftIO (reportGetDX EventsTrackRequest{..})

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
