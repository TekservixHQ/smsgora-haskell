# smsgora: SMS Gora SMS Gateway for Haskell

**SMS Gora SMS Gateway** - official Haskell client by Tekservix. Send and receive SMS through your own Android phones and SIM cards, with delivery reports and signed webhooks.

**You need a Tekservix account with SMS Gora and at least one Android phone linked with the SMS Gora app.** Create an API key under SMS Gora → API keys (it starts with `sgk_`). [Create an account](https://tekservix.com/smsgora) · [API docs](https://tekservix.com/smsgora/docs/)

## Install

```
cabal install --lib smsgora
```

or add `smsgora` to `build-depends`.

## Use

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.SmsGora

main :: IO ()
main = do
  c <- newClient "sgk_your_key"

  _ <- sendSms c ["+250788123456"] "Your order #1042 has shipped."

  -- From a chosen phone and SIM, later
  _ <- sendSmsWith c defaultSmsOptions { smsDeviceId = Just 12, smsSimSlot = Just 1
                                       , smsScheduleAt = Just "2026-12-01T08:00:00Z" }
                   ["+250788123456", "+250722000000"] "Reminder: appointment tomorrow"

  msg <- getMessage c 1873                     -- delivery status
  failed <- listMessages c [("status", "failed"), ("limit", "20")]
  inbox <- listReceivedSms c (Just 12) [("limit", "10")]
  phones <- listDevices c
  print (msg, failed, inbox, phones)
```

Results are aeson `Value`s (the API's `data` field). Errors are thrown as `SmsGoraError` with the HTTP status and message.

Webhooks: `verifyWebhook rawBody signatureHeader secret` checks `X-SmsGora-Signature`.

## More SMS Gora integrations

[npm](https://www.npmjs.com/package/smsgora) · [PyPI](https://pypi.org/project/smsgora/) · [Java/Kotlin](https://central.sonatype.com/artifact/com.tekservix/smsgora) · [All docs](https://tekservix.com/smsgora/docs/)

support@tekservix.com · MIT licensed.
