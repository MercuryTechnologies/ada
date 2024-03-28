{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.ByteString (ByteString)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import GHC.Generics (Generic)
import OpenAI.Client (EngineId(..))
import Options.Generic (ParseRecord(..))

import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI.Client as OpenAI
import qualified Options.Generic

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

data Options = Options
    { openAIAPIKey :: Text
    , slackBearerToken :: ByteString
    } deriving stock (Generic)
      deriving anyclass (ParseRecord)

main :: IO ()
main = do
    Options{..} <- Options.Generic.getRecord "A helpful AI assistant for Mercury engineers"

    manager <- TLS.newTlsManager

    let client = OpenAI.makeOpenAIClient openAIAPIKey manager 3

    let engineId = EngineId "text-davinci-003"

    let textCompletionCreate =
            OpenAI.defaultTextCompletionCreate
                "Write a tagline for an ice cream shop."

    do  result <- OpenAI.completeText client engineId textCompletionCreate

        print result
