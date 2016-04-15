{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.DNSMadeEasy where

import qualified Network.DNS.Types as DNS
-- import Data.Aeson
import Data.Aeson.Types
import Servant.Client
import Servant.API
import Data.Text(Text)
import Data.Proxy
import GHC.Generics
import Control.Monad(mzero)

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

type DNSMadeEasyAPI =
  "dns/managed" :> Get '[JSON] [DNSRecords]


getRecords = client dnsMadeEasyAPI (BaseUrl Https "api.dnsmadeeasy.com/V2.0" 443)

dnsMadeEasyAPI :: Proxy DNSMadeEasyAPI
dnsMadeEasyAPI = Proxy
