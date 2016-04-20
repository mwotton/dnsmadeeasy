{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.Aeson              (eitherDecode)
import           Data.Either             (isRight)
import           Data.Time
import           Network.DNS.DNSMadeEasy
import           Network.DNS.SHAHelper
import           Test.Hspec
import           Text.RawString.QQ       (r)

main :: IO ()
main = hspec spec

spec = describe "dnsmadeeasy" $ do
  it "can get the correct sha1 hmac" $ do
    genSHA1 "abc" "def" `shouldBe` "12554eabbaf7e8e12e4737020f987ca7901016e5"

  it "formats time correctly" $ do
    toRFC1123 (read "2016-04-17 17:06:02.22368 UTC" :: UTCTime)
      `shouldBe` "Sun, 17 Apr 2016 17:06:02 UTC"

  describe "can parse the responses" $ do
    it "Nameservers" $ do
      eitherDecode [r| { "fqdn" : "foo"
                 , "ipv6" : "bar"
                 , "ipv4" : "baz"
                 } |]
        `shouldBe` (Right $ NameServer "foo" "bar" "baz")
    it "ManagedRecords" $ do
      let body =[r|
{
    "pendingActionId": 0,
    "activeThirdParties": [],
    "created": 1457308800000,
    "folderId": 106479,
    "name": "betterteam.com",
    "gtdEnabled": false,
    "processMulti": false,
    "id": 3461121,
    "updated": 1460728358584,
    "delegateNameServers": [
        "ns0.dnsmadeeasy.com.",
        "ns1.dnsmadeeasy.com."
    ],
    "nameServers": [
        {
            "fqdn": "ns0.dnsmadeeasy.com",
            "ipv6": "2600:1800:0::1",
            "ipv4": "208.94.148.2"
        },
        {
            "fqdn": "ns1.dnsmadeeasy.com",
            "ipv6": "2600:1801:1::1",
            "ipv4": "208.80.124.2"
        }
    ]
}|]
      (eitherDecode body :: Either String  ManagedRecords) `shouldSatisfy` isRight

    it "DNSRecord" $ do
      let body= [r|
        { "dynamicDns": false
        , "failed": false
        , "failover": false
        , "gtdLocation": "DEFAULT"
        , "hardLink": false
        , "id": 32342082
        , "monitor": false
        , "name": "www"
        , "source": 1
        , "sourceId": 3461121
        , "ttl": 1800
        , "type": "CNAME"
        , "value": "n.global-ssl.fastly.net."
        } |]
      (eitherDecode body :: Either String DNSRecord)
        `shouldSatisfy` isRight
