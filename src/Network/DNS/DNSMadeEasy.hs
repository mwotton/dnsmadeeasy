{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Network.DNS.DNSMadeEasy
       (module Network.DNS.DNSMadeEasy
       ,module Network.DNS.DNSMadeEasy.Types
       ,BaseUrl
       ,AuthClientData
       ,AuthenticateReq
       )
       where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson                    (Value)
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Time                     (defaultTimeLocale)
import           Data.Time.Clock               (UTCTime, getCurrentTime)
import           Data.Time.Format              (formatTime)
import           Network.DNS.DNSMadeEasy.Types
import           Network.DNS.SHAHelper
import           Network.HTTP.Client           (Manager, managerSetProxy,
                                                useProxy)
import qualified Network.HTTP.Client           as HC
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import qualified Servant.Common.Req            as SCR

newtype DMEAuth = DMEAuth (String,String,UTCTime)

type DNSMadeEasyAPI =
--       "dns/managed"    :> AuthProtect "dmeAuth"
--                        :> Get '[JSON] DNSManagedSingle
--  :<|>
       "dns/managed/id" :>  Capture "domain" Text
                        :> AuthProtect "dmeAuth"
                        :> Get '[JSON] ManagedRecords
  :<|> "dns/managed"    :> Capture "id" RecordID
                        :> "records"
                        :> AuthProtect "dmeAuth"
                        :> Get '[JSON] RecordsFor
   -- Destructive part!
  :<|> "dns/managed"    :> Capture "id" RecordID :> "records"
                        :> ReqBody '[JSON] PostRecord
                        :> AuthProtect "dmeAuth"
                        :> Post '[JSON] DNSRecord
  :<|> "dns/managed"    :> Capture "id" RecordID :> "records"
                        :> Capture "recordId" RecordID
                        :> AuthProtect "dmeAuth"
                        :> Delete '[NoContent] NoContent


getRecord :<|>
  recordsFor :<|>
  postRecord :<|>
  deleteRecord =
  client  dnsMadeEasyAPI


-- | The datatype we'll use to authenticate a request. If we were wrapping
-- something like OAuth, this might be a Bearer token.
-- type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | A method to authenticate a request
authenticateReq :: DMEAuth -> SCR.Req -> SCR.Req
authenticateReq  (DMEAuth (api, secret, time)) req =
  foldl (\r (h,v) -> SCR.addHeader h v r) req (secretHeaders time api secret)


type instance AuthClientData (AuthProtect "dmeAuth") = DMEAuth

secretHeaders :: UTCTime -> String -> String -> [(String,String)]
secretHeaders now api secret =
  [("x-dnsme-apiKey",      api)
  ,("x-dnsme-requestDate", stringNow)
  ,("x-dnsme-hmac",        genSHA1 secret stringNow)
  ]
  where stringNow = toRFC1123 now



-- almost certain this is broken in many ways.
toRFC1123 :: UTCTime -> String
toRFC1123 = formatTime defaultTimeLocale "%a, %e %b %Y %X %Z"

protectedRunner :: (AuthClientData a ~ DMEAuth)
                => String
                -> String
                -> (AuthenticateReq a -> Manager -> BaseUrl -> ExceptT e IO a1)
                -> IO (Either e a1)
protectedRunner api secret function  = do
  manager <- HC.newManager
     (managerSetProxy (useProxy $ HC.Proxy "127.0.0.1" 8888)
     tlsManagerSettings
     )
  runExceptT (authRequest >>= \auth -> function auth manager baseURL)

  where authRequest = do
          t <- liftIO getCurrentTime
          return $ mkAuthenticateReq (DMEAuth (api, secret, t)) authenticateReq
        baseURL = BaseUrl Http "api.dnsmadeeasy.com" 80 "/V2.0"

dnsMadeEasyAPI :: Proxy DNSMadeEasyAPI
dnsMadeEasyAPI = Proxy
