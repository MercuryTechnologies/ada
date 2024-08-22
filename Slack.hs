{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS -Wno-deprecations #-}

module Slack where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Network.Wai (Application, Request)
import Numeric.Natural (Natural)
import Web.FormUrlEncoded (ToForm)

import Cheapskate
    (Block(..), Doc(..), Inline(..), ListType(..), NumWrapper(..), Options(..))
import Data.Aeson
    (FromJSON(..), Options(..), SumEncoding(..), ToJSON(..))
import Servant.API
    ( FormUrlEncoded
    , Header'
    , JSON
    , Optional
    , Post
    , QueryParam'
    , ReqBody
    , Required
    , Strict
    , (:>)
    , (:<|>)
    )

import qualified Cheapskate
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Base16.Types as Base16.Types
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Vector as Vector
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai as Wai

jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions
    { constructorTagModifier = Aeson.camelTo2 '_'
    , fieldLabelModifier = dropWhile (== '_')
    , sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    }

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
    , blocks :: Maybe (Vector Slack.Block)
    , text :: Maybe Text
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

data ConversationsRepliesRequest = ConversationsRepliesRequest
    { channel :: Text
    , ts :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data Message = Message
    { user :: Text
    , text :: Text
    , thread_ts :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data ConversationsRepliesResponse = ConversationsRepliesResponse
    { ok :: Bool
    , error :: Maybe Text
    , messages :: Maybe (Vector Message)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type ConversationsReplies =
        "conversations.replies"
    :>  QueryParam' [Required, Strict] "channel" Text
    :>  QueryParam' [Required, Strict] "ts" Text
    :>  QueryParam' [Optional, Strict] "limit" Natural
    :>  Post '[JSON] ConversationsRepliesResponse

data UsersInfoRequest = UsersInfoRequest
    { user :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToForm)

data Profile = Profile
    { display_name :: Text
    , email :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data User = User
    { profile :: Profile
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data UsersInfoResponse = UsersInfoResponse
    { ok :: Bool
    , error :: Maybe Text
    , user :: Maybe User
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

type UsersInfo =
        "users.info"
    :>  ReqBody '[FormUrlEncoded] UsersInfoRequest
    :>  Post '[JSON] UsersInfoResponse

type Client =
        Header' [Required, Strict] "Authorization" Text
    :>  "api"
    :>  (     AppsConnectionsOpen
        :<|>  ChatPostMessage
        :<|>  ConversationsReplies
        :<|>  UsersInfo
        )

data SocketEvent
    = Hello{ }
    | EventsAPI
        { envelope_id :: Text
        , payload :: Payload
        }
    | Disconnect
    deriving stock (Generic, Show)

instance FromJSON SocketEvent where
    parseJSON = Aeson.genericParseJSON jsonOptions

data Payload = Payload
    { event :: Event
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Event = Event
    { ts :: Text
    , channel :: Text
    , text :: Text
    , user :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

data Acknowledgment = Acknowledgment
    { envelope_id :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

data ServerRequest
    = URLVerification{ token :: Text, challenge :: Text }
    | EventCallback{ event :: Event }
    deriving stock (Generic, Show)

instance FromJSON ServerRequest where
    parseJSON = Aeson.genericParseJSON jsonOptions

data ServerResponse
    = ChallengeResponse{ challenge :: Text }
    | EmptyResponse
    deriving stock (Generic, Show)

instance ToJSON ServerResponse where
    toJSON EmptyResponse = Aeson.Object []
    toJSON ChallengeResponse{ challenge } =
        Aeson.object [ ("challenge", toJSON challenge) ]

type Server =
        ReqBody '[JSON] ServerRequest
    :>  Post '[JSON] ServerResponse

-- https://api.slack.com/authentication/verifying-requests-from-slack
verify :: Text -> Request -> MaybeT IO ByteString
verify signingSecret request = do
    body <- liftIO (Wai.strictRequestBody request)

    Just timestampBytes <- return (lookup "X-Slack-Request-Timestamp" (Wai.requestHeaders request))

    Right timestampText <- return (Text.Encoding.decodeUtf8' timestampBytes)

    timestamp <- Time.parseTimeM True Time.defaultTimeLocale "%s" (Text.unpack timestampText)

    now <- liftIO (POSIX.getPOSIXTime)

    guard (abs (now - timestamp) <= 60 * 5)

    let baseBytes =
            ByteString.concat
                [ "v0:"
                , timestampBytes
                , ":"
                , ByteString.Lazy.toStrict body
                ]

    let signingSecretBytes = Text.Encoding.encodeUtf8 signingSecret

    let hash = SHA256.hmac signingSecretBytes baseBytes

    let base16 = Base16.Types.extractBase16 (Base16.encodeBase16' hash)

    let signature = "v0=" <> base16

    Just xSlackSignature <- return (lookup "x-slack-signature" (Wai.requestHeaders request))

    guard (signature == xSlackSignature)

    return (ByteString.Lazy.toStrict body)

verificationMiddleware :: Text -> Application -> Application
verificationMiddleware signingSecret application request respond = do
    verified <- MaybeT.runMaybeT (verify signingSecret request)

    case verified of
        Just originalRequestBody -> do
            ref <- IORef.newIORef (Just originalRequestBody)

            -- This is a hack to work around the fact that if a signing
            -- middleware consumes the request body then it's not available
            -- for the actual handler.  See:
            --
            -- https://github.com/haskell-servant/servant/issues/1120#issuecomment-1084318908
            let fakeRequestBody = do
                    m <- IORef.readIORef ref
                    case m of
                        Just bytes -> do
                            IORef.writeIORef ref Nothing

                            return bytes

                        Nothing -> do
                            return mempty

            let request' = request{ Wai.requestBody = fakeRequestBody }

            application request' respond

        Nothing -> do
            let response = Wai.responseBuilder HTTP.Types.status400 mempty mempty
            respond response

-- We're only codifying the parts of Slack's Blocks API that we actually use

data PlainText = PlainText{ text :: Text }
    deriving stock (Generic, Show)

instance ToJSON PlainText where
    toJSON = Aeson.genericToJSON jsonOptions
    toEncoding = Aeson.genericToEncoding jsonOptions

data Style = Style
    { bold :: Bool
    , italic :: Bool
    , strike :: Bool
    , code :: Bool
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

defaultStyle :: Style
defaultStyle =
    Style{ bold = False, italic = False, strike = False, code = False }

data ListStyle = Bullet | Ordered
    deriving stock (Generic, Show)

instance ToJSON ListStyle where
    toJSON = Aeson.genericToJSON jsonOptions
    toEncoding = Aeson.genericToEncoding jsonOptions

data RichTextElement
    = Text{ text :: Text, style :: Style }
    | Link{ text :: Text, style :: Style, url :: Text, unsafe :: Bool }
    deriving stock (Generic, Show)

instance ToJSON RichTextElement where
    toJSON = Aeson.genericToJSON jsonOptions
    toEncoding = Aeson.genericToEncoding jsonOptions

instance IsString RichTextElement where
    fromString string = fromText (fromString string)

fromText :: Text -> RichTextElement
fromText string = Text{ text = string, style = defaultStyle }

data RichTextObject
    = RichTextSection{ elements :: Vector RichTextElement }
    | RichTextList{ _elements :: Vector RichTextObject, _style :: ListStyle }
    -- ^ Technically the `RichTextObject`s here can only be `RichTextSection`s.
    | RichTextPreformatted{ elements :: Vector RichTextElement }
    | RichTextQuote{ elements :: Vector RichTextElement }
    deriving (Generic, Show)

instance ToJSON RichTextObject where
    toJSON = Aeson.genericToJSON jsonOptions
    toEncoding = Aeson.genericToEncoding jsonOptions

data Block
    = Divider
    | Header{ text :: PlainText }
    | RichText{ elements :: Vector RichTextObject }
    deriving stock (Generic, Show)

instance ToJSON Slack.Block where
    toJSON = Aeson.genericToJSON jsonOptions
    toEncoding = Aeson.genericToEncoding jsonOptions

enableItalic :: RichTextElement -> RichTextElement
enableItalic richTextElement =
    richTextElement{ style = (style richTextElement){ italic = True } }

enableBold :: RichTextElement -> RichTextElement
enableBold richTextElement =
    richTextElement{ style = (style richTextElement){ bold = True } }

enableStrike :: RichTextElement -> RichTextElement
enableStrike richTextElement =
    richTextElement{ style = (style richTextElement){ strike = True } }

enableCode :: RichTextElement -> RichTextElement
enableCode richTextElement =
    richTextElement{ style = (style richTextElement){ code = True } }

linkTo :: Text -> RichTextElement -> RichTextElement
linkTo url Text{..} = Slack.Link{ unsafe = False, .. }
linkTo url Slack.Link{ url = _, ..} = Slack.Link{..}

htmlToPara :: Text -> Cheapskate.Block
htmlToPara html = Para (pure (RawHtml html))

-- | This is only used for headers, which don't accept any markdown features,
-- so this strips almost all formatting/links and convert newlines to spaces,
-- with the exception of superscript/subscripts, which we can translate to
-- their Unicode equivalents when available.
inlinesToPlainText :: Seq Inline -> Text
inlinesToPlainText = foldMap inlineToPlainText
  where
    inlineToPlainText :: Inline -> Text
    inlineToPlainText (Str string) = string
    inlineToPlainText Space = " "
    inlineToPlainText SoftBreak = " "
    inlineToPlainText LineBreak = " "
    inlineToPlainText (Emph inlines) =
        inlinesToPlainText inlines
    inlineToPlainText (Strong inlines) =
        inlinesToPlainText inlines
    inlineToPlainText (Code string) =
        string
    inlineToPlainText (Cheapskate.Link inlines _uri _maybeTitle) =
        inlinesToPlainText inlines
    inlineToPlainText (Image inlines _uri _maybeTitle) =
        inlinesToPlainText inlines
    inlineToPlainText (Entity string) = string
    inlineToPlainText (RawHtml string) = string

-- An `Inline` is basically `mmark`'s version of a span and a `RichTextElement`
-- is basically Slack's version of a span, so this function essentially
-- converts from Markdown spans to Slack spans.
--
-- The main thing to keep in mind here is that `mmark`'s spans can be nested
-- because it can parse things like:
--
-- > **foo `bar`**
--
-- … which translates to something like `Strong [ "foo ", CodeSpan "bar" ]`.
--
-- However, Slack spans are not nested, so when we convert to Slack we have to
-- turn `mmark`'s nested formatting specifiers and flatten them to something
-- like:
--
-- > [ Text{ text = "foo ", style = defaultStyle{ bold = True } }
-- > , Text{ text = "bar" , style = defaultStyle{ bold = True, code = True } }
-- > ]
--
inlinesToRichTextElements :: Seq Inline -> [RichTextElement]
inlinesToRichTextElements = foldMap inlineToRichTextElements
  where
    inlineToRichTextElements :: Inline -> [RichTextElement]
    inlineToRichTextElements (Str string) =
        [ fromText string ]
    inlineToRichTextElements Space =
        [ " " ]
    inlineToRichTextElements SoftBreak =
        [ " " ]
    inlineToRichTextElements LineBreak =
        [ "\n" ]
    inlineToRichTextElements (Emph inlines) =
        fmap enableItalic (inlinesToRichTextElements inlines)
    inlineToRichTextElements (Strong inlines) =
        fmap enableBold (inlinesToRichTextElements inlines)
    inlineToRichTextElements (Code string) =
        [ Text{ text = string, style = defaultStyle{ code = True } } ]
    inlineToRichTextElements (Cheapskate.Link inlines uri _maybeTitleText) =
        fmap (linkTo uri) (inlinesToRichTextElements inlines)
    inlineToRichTextElements (Image inlines uri _maybeTitleText) =
        fmap (linkTo uri) (inlinesToRichTextElements inlines)
    inlineToRichTextElements (Entity string) =
        [ fromText string ]
    inlineToRichTextElements (RawHtml string) =
        [ fromText string ]

blocksToRichTextElements :: Seq Cheapskate.Block -> [RichTextElement]
blocksToRichTextElements = foldMap blockToRichTextElements

-- | Unlike markdown, Slack's Blocks API *does not* permit nesting features
--   arbitrarily.  This means that once we start going one level deep (like
--   inside of a list or inside quotes), we need to start simulating markdown
--   features as their textual markdown representation.  For example, we
--   convert headers to "#"s and convert lists to their textual representation
--   instead of using Slack's native support for ordered/unordered lists.
blockToRichTextElements :: Cheapskate.Block -> [RichTextElement]
blockToRichTextElements (Para inlines) =
    inlinesToRichTextElements inlines
blockToRichTextElements (Cheapskate.Header indentation inlines) =
    fmap enableBold (hashes : " " : inlinesToRichTextElements inlines) <>  [ "\n" ]
  where
    hashes = fromString (replicate indentation '#')
blockToRichTextElements (Blockquote blocks) =
    blocksToRichTextElements blocks
blockToRichTextElements (List _ (Numbered wrapper startingIndex) items) = "\n" : do
    (index, blocks) <- zip [ startingIndex .. ] (Foldable.toList items)
    let separator = case wrapper of
            PeriodFollowing -> "."
            ParenFollowing -> ")"
    fromString (show index <> separator <> " ") : blocksToRichTextElements blocks <> [ "\n" ]
blockToRichTextElements (List _ (Cheapskate.Bullet bullet) items) = "\n" : do
    blocks <- Foldable.toList items
    fromString (bullet : " ") : blocksToRichTextElements blocks <> [ "\n" ]
blockToRichTextElements (CodeBlock _maybeInfo string) =
    [ Text{ text = "\n" <> string, style = defaultStyle{ code = True } }
    ]
blockToRichTextElements (HtmlBlock html) =
    blockToRichTextElements (htmlToPara html)
blockToRichTextElements HRule =
    [ "* * *\n" ]

itemToRichTextSection :: Seq Cheapskate.Block -> RichTextObject
itemToRichTextSection blocks = RichTextSection
    { elements = Vector.fromList (blocksToRichTextElements blocks) }

-- Yeah, they're both called blocks.  Go figure.
blockToBlock :: Cheapskate.Block -> Slack.Block
blockToBlock (Para inlines) = RichText
    { elements =
        [ RichTextSection
            { elements =
                Vector.fromList (inlinesToRichTextElements inlines)
            }
        ]
    }
-- Unfortunately, Slack's Blocks API doesn't support multiple levels
-- of headers, so we have to translate all header levels in the exact
-- same way.
blockToBlock (Cheapskate.Header _ inlines) =
    Slack.Header { text = PlainText{ text = inlinesToPlainText inlines } }
blockToBlock (Blockquote blocks) = RichText
    { elements =
        [ RichTextQuote
            { elements = Vector.fromList (blocksToRichTextElements blocks) }
        ]
    }
blockToBlock (List _ (Numbered _ _) items) = RichText
    { elements =
        [ RichTextList
            { _elements =
                Vector.fromList (fmap itemToRichTextSection (Foldable.toList items))
            , _style = Ordered
            }
        ]
    }
blockToBlock (List _ (Cheapskate.Bullet _) items) = RichText
    { elements =
        [ RichTextList
            { _elements =
                Vector.fromList (fmap itemToRichTextSection (Foldable.toList items))
            , _style = Slack.Bullet
            }
        ]
    }
blockToBlock (CodeBlock _maybeInfo string) = RichText
    { elements = [ RichTextPreformatted { elements = [ fromText string ] } ] }
blockToBlock (HtmlBlock html) = blockToBlock (htmlToPara html)
blockToBlock HRule = Divider

docToBlocks :: Doc -> Vector Slack.Block
docToBlocks (Doc _ blocks) =
    Vector.fromList (map blockToBlock (Foldable.toList blocks))

markdownToBlocks :: Text -> Vector Slack.Block
markdownToBlocks text = docToBlocks (Cheapskate.markdown options text)
  where
    options = Options
        { sanitize = True
        , allowRawHtml = False
        , preserveHardBreaks = False
        , debug = False
        }
