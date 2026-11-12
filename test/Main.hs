{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (try)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BC
import Network.SmsGora
import System.Exit (exitFailure)

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
  pure ok

main :: IO ()
main = do
  let body = BC.pack "{\"event\":\"sms.received\"}"
      -- HMAC-SHA256 of body with secret "whsec_test"
      good = "sha256=313fe9b54923f49c8b94d50c5b96fa44f3f5a7ca90ef52fd55afb2bfd3ae8f60"
  r0 <- check "accepts the right signature" (verifyWebhook body good "whsec_test")
  r1 <- check "rejects a wrong secret" (not (verifyWebhook body good "other_secret"))
  r2 <- check "rejects an empty signature" (not (verifyWebhook body "" "whsec_test"))
  c <- newClient "sgk_invalid_haskell_test"
  res <- try (getAccount c)
  r3 <- check "invalid key gives a 401 SmsGoraError" $ case res of
    Left e -> errorStatus e == 401
    Right _ -> False
  unless (and [r0, r1, r2, r3]) exitFailure
