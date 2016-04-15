{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DNS.DNSMadeEasy where

import qualified Network.DNS.Types as DNS
-- import Data.Aeson
import Data.Aeson.Types
import Servant.Client
import Servant.Common.Req(Req(..))
import Servant.API
import Data.Text(Text)
import Data.Proxy
import GHC.Generics
import Control.Monad(mzero)
import Network.HTTP.Client(Manager)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class(liftIO)
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Servant.Common.Req         as SCR

-- important operations
--   find the MX records for a particular domain name
--   update/insert MX records for a domain name

-- we don't do anything useful with the totalPages data.
-- not sure what it actually means.
data DNSRecords =
  DNSRecords
  { dmeTotalRecords :: Int
  , dmeTotalPages   :: Int
  , dmeRecords :: [DNSRecord]
  } deriving Generic

instance FromJSON DNSRecords where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

data Location = DEFAULT
              | US_EAST
              | US_WEST
              | EUROPE

data Source = DomainSpecific | FromTemplate
newtype RecordID = RecordID Integer

data HTTPRed =
  HTTPRed
  { dmeHTTPRedDesc         :: Text
  , dmeHTTPRedKeywords     :: Text
  , dmeHTTPRedTitle        :: Text
  , dmeHTTPRedRedirectType :: RedirectType
  , dmeHTTPRedHardlink     :: Bool
  }

data SRV =
  SRV
  { dmeWeight      :: Integer
  , dmePriority    :: Integer
  , dmePort        :: Integer
  }

data RedirectType = HiddenFrameMasked
                  | ThreeOhOne
                  | ThreeOhTwo  -- sue me

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
  }


instance FromJSON DNSRecord where
  parseJSON (Object o) = error "stuff"
  parseJSON _ = mzero

type DNSMadeEasyAPI = "dns/managed"                      :> Get '[JSON] [DNSRecords]
--                 :<|> "dns/managed" :> Capture "id" Text :> Get '[JSON] DNSRecord

getRecords m = client dnsMadeEasyAPI m  (BaseUrl Https "api.dnsmadeeasy.com/V2.0" 443 "/")


-- | The datatype we'll use to authenticate a request. If we were wrapping
-- something like OAuth, this might be a Bearer token.
type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | A method to authenticate a request
authenticateReq :: (String,String,UTCTime) -> Req -> Req
authenticateReq  (api,secret, time) req = foldl (\r (h,v) -> SCR.addHeader h v r)  req (secretHeaders time api secret)


secretHeaders :: UTCTime -> String -> String -> [(String,String)]
secretHeaders = undefined


runProtected function = runExceptT (liftIO getCurrentTime >>= \d -> function (mkAuthenticateReq ("api","secret", d) authenticateReq))

dnsMadeEasyAPI :: Proxy DNSMadeEasyAPI
dnsMadeEasyAPI = Proxy
