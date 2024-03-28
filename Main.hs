{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (many)
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Vector (Vector)
import Options.Applicative (Parser, ParserInfo, ParserPrefs(..))
import Servant.API ((:<|>)(..))

import OpenAI
    ( Choice(..)
    , CompletionRequest(..)
    , CompletionResponse(..)
    , EmbeddingRequest(..)
    , Message(..)
    )

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI
import qualified Options.Applicative as Options
import qualified Servant.Client as Client

example :: Text
example = [__i|
Howdy howdy, I’m working through migrating from an Intel MBP to an ARM one here. One thing I just hit was where it tried to run `npm` after installing `fnm`:
```
Write `zsh` configuration? ([y]es, [n]o, yes to [a]ll) y
• Writing new shell configuration
  shell=zsh
  path="/Users/rjmholt/.zprofile"
• Backing up
  path="/Users/rjmholt/.zprofile"
  backup_path="/Users/rjmholt/.zprofile.orig"
• `yarn` is already installed

⚠ Step `node` failed (Install `puppeteer`):
     0: Failed to list globally installed `npm` packages
     1: Command failed: `/bin/zsh -c 'npm list --global --json'`
     2: /bin/zsh failed: exit status: 127

  Location:
     src/node/util/list_global_packages.rs:15

  Stderr:
     zsh:1: command not found: npm

    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ SPANTRACE
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

     0: bootstrap_mercury::runner::run_step with step=Node description=Install
  `puppeteer`
        at src/runner.rs:22
```
Once I ran `eval "$(fnm env --use-on-cd)"` in my shell, it worked again. So I just wanted to raise that in case it’s something that bootstrap-mercury should do in general rather than an issue idiosyncratic to my scenario
|]

data Mode = Index{ paths :: Vector FilePath } | Query

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

parseQuery :: Parser Mode
parseQuery = pure Query

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

main :: IO ()
main = do
    Options{..} <- Options.customExecParser parserPrefs parseOptionsInfo

    manager <- TLS.newTlsManager

    baseUrl <- Client.parseBaseUrl "https://api.openai.com"

    let clientEnv = Client.mkClientEnv manager baseUrl

    let (embeddings :<|> completions) = OpenAI.getClient header
          where
            header = "Bearer " <> openAIAPIKey

    let clientM = case mode of
            Index paths -> do
                forM_ paths \path -> do
                    input <- liftIO (Text.IO.readFile path)

                    let model = "text-embedding-3-large"

                    let embeddingRequest = EmbeddingRequest{..}

                    embeddingResponse <- embeddings embeddingRequest

                    liftIO (print embeddingResponse)
            Query -> do
                let completionRequest = CompletionRequest{..}
                      where
                       message = Message{..}
                         where
                           role = "user"

                           content = example

                       messages = [ message ]

                       max_tokens = Just 1024

                       model = "gpt-4"

                CompletionResponse{..} <- completions completionRequest

                let toContent :: Choice -> Text
                    toContent Choice{ message = Message{..} } = content

                let toChunk :: Int -> Choice -> Text
                    toChunk index choice = [__i|
                        Choice \##{index}:

                        #{toContent choice}
                    |]

                liftIO case choices of
                    [ choice ] -> do
                        Text.IO.putStrLn (toContent choice)
                    _ -> do
                        let chunks = Vector.imap toChunk choices

                        let text =
                                Text.intercalate "\n\n" (Vector.toList chunks)

                        Text.IO.putStr text

    result <- Client.runClientM clientM clientEnv

    case result of
        Left  clientError -> Exception.throwIO clientError
        Right x           -> return x
