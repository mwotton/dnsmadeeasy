{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.DNS.DNSMadeEasy
       (module Network.DNS.DNSMadeEasy
       ,module Network.DNS.DNSMadeEasy.Types)

       where

-- import Data.Aeson

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Proxy
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Network.HTTP.Client        (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Common.Req         (Req (..))
import qualified Servant.Common.Req         as SCR
import Data.Yaml
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Client (withManager, defaultManagerSettings)
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Network.DNS.SHAHelper
import Data.Time.Format (formatTime)
import Data.Time (TimeZone(..))
import Data.Time (defaultTimeLocale)
import Network.HTTP.Client (managerSetProxy)
import Network.HTTP.Client (useProxy)
import qualified Network.HTTP.Client as HC
import Network.DNS.DNSMadeEasy.Types
import           Data.Text                  (Text)

-- important operations
--   find the MX records for a particular domain name
--   update/insert MX records for a domain name





newtype DMEAuth = DMEAuth (String,String,UTCTime)

type DNSMadeEasyAPI = "dns/managed"
                      :> AuthProtect "dmeAuth" :> Get '[JSON] DNSRecords
                 :<|> "dns/managed/id" :>  Capture "domain" Text
                      :> AuthProtect "dmeAuth" :> Get '[JSON] DNSRecord
                 :<|> "dns/managed" :> Capture "id" Text :> "records"
                      :> AuthProtect "dmeAuth" :> Get '[JSON] DNSRecords

getRecords :<|> getRecord :<|> recordsFor = client  dnsMadeEasyAPI

-- | The datatype we'll use to authenticate a request. If we were wrapping
-- something like OAuth, this might be a Bearer token.
-- type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | A method to authenticate a request
authenticateReq :: DMEAuth -> Req -> Req
authenticateReq  (DMEAuth (api, secret, time)) req =
  foldl (\r (h,v) -> SCR.addHeader h v r)  req (secretHeaders time api secret)


type instance AuthClientData (AuthProtect "dmeAuth") = DMEAuth

secretHeaders :: UTCTime -> String -> String -> [(String,String)]
secretHeaders now api secret =
  [("Accept",              "application/json")
  ,("x-dnsme-apiKey",      api)
  ,("x-dnsme-requestDate", stringNow)
  ,("x-dnsme-hmac",        genSHA1 secret stringNow)
  ]
  where stringNow = toRFC1123 now



-- almost certain this is broken in many ways.
toRFC1123 :: UTCTime -> String
toRFC1123 = formatTime defaultTimeLocale "%a, %e %b %Y %X %Z"



protectedRunner api secret function  =  -- withManager tlsManagerSettings $
  -- \manager -> do
  withManager
--    (managerSetProxy (useProxy $ HC.Proxy "127.0.1.1" 8000)
     tlsManagerSettings $ \manager -> do
      t <- getCurrentTime
      runExceptT (function (authRequest t) manager baseURL)

  where authRequest t = mkAuthenticateReq (DMEAuth (api, secret, t)) authenticateReq
        baseURL = BaseUrl Https "api.dnsmadeeasy.com" 443 "/V2.0"

dnsMadeEasyAPI :: Proxy DNSMadeEasyAPI
dnsMadeEasyAPI = Proxy
