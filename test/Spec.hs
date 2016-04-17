{-# LANGUAGE OverloadedStrings #-}
import           Network.DNS.DNSMadeEasy
import           Network.DNS.SHAHelper

import Test.Hspec
import Data.Time
import Data.Aeson (decode)

main :: IO ()
main = hspec spec

spec = describe "dnsmadeeasy" $ do
  it "can get the correct sha1 hmac" $ do
    genSHA1 "abc" "def" `shouldBe` "12554eabbaf7e8e12e4737020f987ca7901016e5"

  it "formats time correctly" $ do
    toRFC1123 (read "2016-04-17 17:06:02.22368 UTC" :: UTCTime)
      `shouldBe` "Sun, 17 Apr 2016 17:06:02 UTC"

  it "can parse the responses" $ do
    let body = "{\"totalRecords\":1,\"totalPages\":1,\"data\":[{\"created\":1457308800000,\"folderId\":106479,\"gtdEnabled\":false,\"updated\":1460728358584,\"processMulti\":false,\"activeThirdParties\":[],\"pendingActionId\":0,\"name\":\"betterteam.com\",\"id\":3461121}],\"page\":0}"

    decode body `shouldBe` (Just $
      DNSRecords 1 1 [DNSRecord { dmeName = "betterteam.com"}])
