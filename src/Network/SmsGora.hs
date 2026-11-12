{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.SmsGora
-- Description : SMS Gora SMS Gateway client by Tekservix
--
-- SMS Gora SMS Gateway: send and receive SMS through your own Android phones.
--
-- You need a Tekservix account with SMS Gora and an API key (it starts with @sgk_@).
-- Create one at <https://tekservix.com/smsgora>. API reference:
-- <https://tekservix.com/smsgora/docs/>.
--
-- > import Network.SmsGora
-- >
-- > main :: IO ()
-- > main = do
-- >   c <- newClient "sgk_your_key"
-- >   r <- sendSms c ["+250788123456"] "Your order has shipped"
-- >   print r
module Network.SmsGora
  ( -- * Client
    Client
  , newClient
  , newClientWith
  , defaultBaseUrl
    -- * Sending
  , SmsOptions (..)
  , defaultSmsOptions
  , sendSms
  , sendSmsWith
  , sendBulkSms
    -- * Messages
  , getMessage
  , cancelMessage
  , listMessages
  , listReceivedSms
  , getBatch
    -- * Phones and account
  , listDevices
  , getDevice
  , getAccount
    -- * Webhooks
  , listWebhooks
  , createWebhook
  , deleteWebhook
  , verifyWebhook
    -- * Anything else
  , request
    -- * Errors
  , SmsGoraError (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (renderQuery, statusCode)

-- | Connection to the SMS Gora API.
data Client = Client
  { clientKey :: !B.ByteString
  , clientBase :: !String
  , clientManager :: !Manager
  }

-- | @https://tekservix.com/smsgora/api/v1@
defaultBaseUrl :: String
defaultBaseUrl = "https://tekservix.com/smsgora/api/v1"

-- | Client for the live API.
newClient :: Text -> IO Client
newClient key = newClientWith key defaultBaseUrl

-- | Client with another base URL (for tests or a proxy).
newClientWith :: Text -> String -> IO Client
newClientWith key base = Client (TE.encodeUtf8 key) (dropWhileEnd' (== '/') base) <$> newTlsManager
  where dropWhileEnd' p = reverse . dropWhile p . reverse

-- | An API error: HTTP status and the message from the API.
data SmsGoraError = SmsGoraError
  { errorStatus :: !Int
  , errorMessage :: !Text
  , errorBody :: !BL.ByteString
  }
  deriving (Show)

instance Exception SmsGoraError

-- | Calls any endpoint and returns the @data@ part of the answer.
--
-- > request c "GET" "/batches/311" Nothing
request :: Client -> B.ByteString -> String -> Maybe Value -> IO Value
request c method path body = do
  req0 <- parseRequest (clientBase c ++ path)
  let req =
        req0
          { method = method
          , requestHeaders =
              [ ("x-api-key", clientKey c)
              , ("Accept", "application/json")
              , ("User-Agent", "smsgora-haskell/1.0.0")
              ]
                ++ maybe [] (const [("Content-Type", "application/json")]) body
          , requestBody = maybe (requestBody req0) (RequestBodyLBS . A.encode) body
          }
  res <- httpLbs req (clientManager c)
  let code = statusCode (responseStatus res)
      raw = responseBody res
      parsed = A.decode raw :: Maybe Value
  if code >= 400
    then throwIO (SmsGoraError code (maybe (T.pack ("HTTP " ++ show code)) messageOf parsed) raw)
    else pure (maybe Null dataOf parsed)
  where
    dataOf (Object o) | Just d <- KM.lookup "data" o = d
    dataOf v = v
    messageOf (Object o) | Just (String m) <- KM.lookup "message" o = m
    messageOf _ = "request failed"

-- | Optional settings for 'sendSmsWith'.
data SmsOptions = SmsOptions
  { smsDeviceId :: Maybe Int -- ^ send from this phone (default: your best online phone)
  , smsSimSlot :: Maybe Int -- ^ 0 = SIM 1, 1 = SIM 2
  , smsScheduleAt :: Maybe Text -- ^ ISO 8601 time to send later
  }

defaultSmsOptions :: SmsOptions
defaultSmsOptions = SmsOptions Nothing Nothing Nothing

-- | One text to up to 1,000 numbers, from your best online phone.
sendSms :: Client -> [Text] -> Text -> IO Value
sendSms c = sendSmsWith c defaultSmsOptions

-- | Like 'sendSms', choosing the phone, SIM or a later time.
sendSmsWith :: Client -> SmsOptions -> [Text] -> Text -> IO Value
sendSmsWith c o to msg =
  request c "POST" path . Just . object $
    ["recipients" .= to, "message" .= msg]
      ++ catMaybes [("sim_slot" .=) <$> smsSimSlot o, ("schedule_at" .=) <$> smsScheduleAt o]
  where
    path = maybe "/send-sms" (\d -> "/devices/" ++ show d ++ "/send-sms") (smsDeviceId o)

-- | Different texts for different people (up to 5,000 recipients). Each message is an
-- object with @recipients@ and an optional @message@; the template is used when it has none.
sendBulkSms :: Client -> Int -> [Value] -> Maybe Text -> IO Value
sendBulkSms c deviceId msgs template =
  request c "POST" ("/devices/" ++ show deviceId ++ "/send-bulk-sms") . Just . object $
    ("messages" .= msgs) : maybe [] (\t -> ["messageTemplate" .= t]) template

-- | One message; status is queued, dispatched, sent, delivered, failed or cancelled.
getMessage :: Client -> Int -> IO Value
getMessage c i = request c "GET" ("/messages/" ++ show i) Nothing

-- | Cancels a queued or scheduled message.
cancelMessage :: Client -> Int -> IO Value
cancelMessage c i = request c "POST" ("/messages/" ++ show i ++ "/cancel") Nothing

-- | Messages, filtered: direction, status, device_id, phone, since, page, limit.
listMessages :: Client -> [(Text, Text)] -> IO Value
listMessages c q = request c "GET" ("/messages" ++ query q) Nothing

-- | Incoming SMS for one phone, or all phones with 'Nothing'.
listReceivedSms :: Client -> Maybe Int -> [(Text, Text)] -> IO Value
listReceivedSms c dev q = request c "GET" (base ++ query q) Nothing
  where base = maybe "/received-sms" (\d -> "/devices/" ++ show d ++ "/get-received-sms") dev

-- | A bulk batch.
getBatch :: Client -> Int -> IO Value
getBatch c i = request c "GET" ("/batches/" ++ show i) Nothing

-- | Your linked phones.
listDevices :: Client -> IO Value
listDevices c = request c "GET" "/devices" Nothing

-- | One phone.
getDevice :: Client -> Int -> IO Value
getDevice c i = request c "GET" ("/devices/" ++ show i) Nothing

-- | Plan, limits and usage.
getAccount :: Client -> IO Value
getAccount c = request c "GET" "/account" Nothing

-- | Your webhooks.
listWebhooks :: Client -> IO Value
listWebhooks c = request c "GET" "/webhooks" Nothing

-- | Adds a webhook for the given events (e.g. @sms.received@, @sms.failed@), optionally for one phone.
createWebhook :: Client -> Text -> [Text] -> Maybe Int -> IO Value
createWebhook c url events dev =
  request c "POST" "/webhooks" . Just . object $
    ["url" .= url, "events" .= events] ++ maybe [] (\d -> ["device_id" .= d]) dev

-- | Removes a webhook.
deleteWebhook :: Client -> Int -> IO Value
deleteWebhook c i = request c "DELETE" ("/webhooks/" ++ show i) Nothing

-- | Checks the @X-SmsGora-Signature@ header (@sha256=@ + hex HMAC-SHA256 of the raw body)
-- with your webhook secret, in constant time.
verifyWebhook :: B.ByteString -> Text -> Text -> Bool
verifyWebhook rawBody signature secret =
  let expected = "sha256=" <> B16.encode (SHA256.hmac (TE.encodeUtf8 secret) rawBody)
      given = TE.encodeUtf8 (T.strip signature)
   in B.length expected == B.length given
        && foldr (.|.) 0 (B.zipWith xor expected given) == 0

query :: [(Text, Text)] -> String
query [] = ""
query q = BC.unpack (renderQuery True [(TE.encodeUtf8 k, Just (TE.encodeUtf8 v)) | (k, v) <- q])
