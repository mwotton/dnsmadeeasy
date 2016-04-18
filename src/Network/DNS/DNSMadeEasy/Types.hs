{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.DNSMadeEasy.Types where

import qualified Network.DNS.Types as DNS
import           Data.Text                  (Text)
import           Data.Aeson.Types
import           GHC.Generics
import           Control.Monad              (mzero)

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
  parseJSON (Object o) = error (show ("dnsrecord",o))
  parseJSON _ = mzero
