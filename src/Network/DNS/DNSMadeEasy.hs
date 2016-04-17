{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.DNS.DNSMadeEasy where

import qualified Network.DNS.Types          as DNS
-- import Data.Aeson
import           Control.Monad              (mzero)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           GHC.Generics
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

-- important operations
--   find the MX records for a particular domain name
--   update/insert MX records for a domain name




-- we don't do anything useful with the totalPages data.
-- not sure what it actually means.
data DNSRecords =
  DNSRecords
  { dmeTotalRecords :: Int
  , dmeTotalPages   :: Int
  , dmeRecords      :: [DNSRecord]
  } deriving (Generic, Show, Eq)

instance FromJSON DNSRecords where
  parseJSON (Object o) =
    DNSRecords <$> o .: "totalRecords"
               <*> o .: "totalPages"
               <*> ( o .: "data" >>= mapM parseJSON)
--     genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

data Location = DEFAULT
              | US_EAST
              | US_WEST
              | EUROPE
              deriving (Show,Eq)
data Source = DomainSpecific | FromTemplate
   deriving (Show,Eq)

newtype RecordID = RecordID Integer
   deriving (Show,Eq)

data HTTPRed =
  HTTPRed
  { dmeHTTPRedDesc         :: Text
  , dmeHTTPRedKeywords     :: Text
  , dmeHTTPRedTitle        :: Text
  , dmeHTTPRedRedirectType :: RedirectType
  , dmeHTTPRedHardlink     :: Bool
  }
   deriving (Show,Eq)
data SRV =
  SRV
  { dmeWeight   :: Integer
  , dmePriority :: Integer
  , dmePort     :: Integer
  } deriving (Show,Eq)

data RedirectType = HiddenFrameMasked
                  | ThreeOhOne
                  | ThreeOhTwo  -- sue me
                  deriving (Show,Eq)
data DNSRecord =
  DNSRecord
  { dmeName        :: Text
  , dmeValue       :: Text -- FIXME this sucks
  , dmeSource      :: Source
  , dmeID          :: RecordID
  , dmeType        :: DNS.TYPE
  , dmeDynamicDns  :: Bool
  , dmeTTL         :: Integer -- in seconds? could convert to
                              -- duration.
  , dmeMonitor     :: Bool
  , dmeFailover    :: Bool
  , dmeFailed      :: Bool
  , dmeGtdLocation :: Location
  , dmePassword    :: Maybe Text
  , dmeHTTPRED     :: Maybe HTTPRed
  , dmeMxLevel     :: Maybe Integer   -- arguably this should be in the
                                      -- description of MX

  , dmeSRV         :: Maybe SRV
  } deriving (Show,Eq)


instance FromJSON DNSRecord where
  parseJSON (Object o) = error "stuff"
  parseJSON _ = mzero


type DMEAuth = (String,String,UTCTime)
type DNSMadeEasyAPI = "dns/managed"  :> AuthProtect "dmeAuth" :>  Get '[JSON] [DNSRecords]
--                 :<|> "dns/managed" :> Capture "id" Text :> Get '[JSON] DNSRecord


getRecords = client  dnsMadeEasyAPI --  _foo _m  (BaseUrl Https "api.dnsmadeeasy.com/V2.0" 443 "/")


-- | The datatype we'll use to authenticate a request. If we were wrapping
-- something like OAuth, this might be a Bearer token.
-- type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | A method to authenticate a request
authenticateReq :: (String,String,UTCTime) -> Req -> Req
authenticateReq  (api,secret, time) req = foldl (\r (h,v) -> SCR.addHeader h v r)  req (secretHeaders time api secret)


type instance AuthClientData (AuthProtect "dmeAuth") = DMEAuth

secretHeaders :: UTCTime -> String -> String -> [(String,String)]
secretHeaders now api secret =
  [("x-dnsme-apiKey",      api)
  ,("x-dnsme-requestDate", stringNow)
  ,("x-dnsme-hmac",        genSHA1 secret stringNow)
  ]
  where stringNow = toRFC1123 now


-- genSHA1 secret string = listFromOctets (hmac_sha1 (listToOctets secret)  (listToOctets string))

-- runProtected (api,secret) function =
--   runExceptT (liftIO getCurrentTime >>=
--               \d -> function
--                     (mkAuthenticateReq  (api,secret,d) authenticateReq))

sometime :: UTCTime
sometime = read "2016-04-17 16:54:37.110728 UTC"

-- almost certain this is broken in many ways.
toRFC1123 :: UTCTime -> String
toRFC1123 = formatTime defaultTimeLocale "%a, %e %b %Y %X %Z"


getRecordsProtected :: String -> String -> IO (Either ServantError [DNSRecords])
getRecordsProtected api secret  =  -- withManager tlsManagerSettings $
  -- \manager -> do
  withManager
    -- (managerSetProxy (useProxy $ HC.Proxy "127.0.1.1" 8000)  defaultManagerSettings ) $ \manager -> do
    tlsManagerSettings $ \manager -> do
    t <- getCurrentTime
    runExceptT (getRecords (authRequest t) manager baseURL)

      where authRequest t = mkAuthenticateReq (api, secret, t) authenticateReq
            baseURL = BaseUrl Https "api.dnsmadeeasy.com" 443 "/V2.0"

dnsMadeEasyAPI :: Proxy DNSMadeEasyAPI
dnsMadeEasyAPI = Proxy
