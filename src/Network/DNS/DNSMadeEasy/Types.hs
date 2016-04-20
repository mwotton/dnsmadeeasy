{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.DNSMadeEasy.Types where

import           Control.Monad              (mzero)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                  (toLower)
import           Data.Monoid                ((<>))
import           Data.Scientific            (toBoundedInteger)
import           Data.Text                  (Text)
import           GHC.Generics
import qualified Network.DNS.Types          as DNS
import           Servant.API                (ToHttpApiData, toUrlPiece)

-- we don't do anything useful with the totalPages data.
-- not sure what it actually means.
data DNSRecords =
  DNSRecords
  { dmeTotalRecords :: Int
  , dmeTotalPages   :: Int
  , dmeRecords      :: [DNSRecordsEntry]
  } deriving (Generic, Show, Eq)

data DNSRecordsEntry = DNSRecordsEntry
  deriving (Generic, Show, Eq)

instance FromJSON DNSRecords where
  parseJSON (Object o) = error $ "dnsrecords: " <> BL.unpack (encodePretty o)
--     DNSRecords <$> o .: "totalRecords"
--                <*> o .: "totalPages"
--                <*> ( o .: "data" >>= mapM parseJSON)
-- --     genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })



data Location = DEFAULT
              | US_EAST
              | US_WEST
              | EUROPE
              deriving (Generic,Show,Eq)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Location where
  toJSON = genericToJSON defaultOptions



data Source = DomainSpecific | FromTemplate
   deriving (Show,Eq)

instance FromJSON Source where
  parseJSON (Number 1) = return DomainSpecific
  parseJSON (Number 0) = return FromTemplate
  parseJSON _ = mzero

newtype RecordID = RecordID {
  unRecordID :: Int
  }
   deriving (Show,Eq)

instance FromJSON RecordID where
  parseJSON (Number s) = case toBoundedInteger s of
    Nothing -> mzero
    Just i  -> return $ RecordID i
  parseJSON _ = mzero


data HTTPRed =
  HTTPRed
  { dmeHTTPRedDesc         :: Text
  , dmeHTTPRedKeywords     :: Text
  , dmeHTTPRedTitle        :: Text
  , dmeHTTPRedRedirectType :: RedirectType
  , dmeHTTPRedHardlink     :: Bool
  }
   deriving (Generic,Show,Eq)

instance FromJSON HTTPRed where
  parseJSON = genericParseJSON (mkOptions 10)

instance FromJSON RedirectType where
  parseJSON o = error (show("redirect",o))

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
  { dmeDynamicDns  :: Bool
  , dmeFailed      :: Bool
  , dmeFailover    :: Bool
  , dmeGtdLocation :: Location
  , dmeHTTPRED     :: Maybe HTTPRed
  , dmeId          :: RecordID
  , dmeMonitor     :: Bool
  , dmeMxLevel     :: Maybe Integer   -- arguably this should
  , dmeName        :: Text
  , dmePassword    :: Maybe Text
  , dmeSource      :: Source
  , dmeSRV         :: Maybe SRV
  , dmeTtl         :: Integer -- in seconds? could convert to
  , dmeType        :: Text
  , dmeValue       :: Text -- FIXME this su

  } deriving (Generic,Show,Eq)

instance FromJSON DNSRecord where
  parseJSON = genericParseJSON (mkOptions 3)

instance FromJSON SRV where
  parseJSON = error "SRV parse"

-- not really sure why what we post is so different to
-- what we get.
data PostRecord =
  PostRecord
  { postName        :: Text
    -- this is really pretty dreadful,
    -- probably needs hand-crafted ToJSON instances
    -- and a proper datatype
  , postType        :: Text
  , postMxLevel     :: Maybe Int
  , postValue       :: Text
  , postTtl         :: Integer
  , postGtdLocation :: Location
  } deriving (Generic, Show,Eq)

instance ToJSON PostRecord where
  toJSON = genericToJSON (mkOptions 4)


--    body = {"name" => name, "type" => type, "value" => value, "ttl" => 3600, "gtdLocation" => "DEFAULT"}


data ManagedRecords = ManagedRecords
  { mrecPendingActionId     :: Integer
  , mrecActiveThirdParties  :: [Text]
  , mrecCreated             :: Integer -- probably a date
  , mrecFolderId            :: Integer
  , mrecName                :: Text
  , mrecProcessMulti        :: Bool
  , mrecId                  :: RecordID
  , mrecUpdated             :: Integer
  , mrecDelegateNameServers :: [Text]
  , mrecNameServers         :: [NameServer]

--  , mrecGtdEnabled
} deriving (Generic, Eq, Show)

data NameServer = NameServer
  { nsFqdn :: Text
  , nsIpv6 :: Text
  , nsIpv4 :: Text
  } deriving (Generic, Eq, Show)


instance FromJSON NameServer where
  parseJSON = genericParseJSON (mkOptions 2)

mkOptions n = defaultOptions {
  fieldLabelModifier = decapitalise . drop n
}

decapitalise (x:xs) = toLower x:xs
decapitalise x = x

instance FromJSON ManagedRecords where
  parseJSON = genericParseJSON (mkOptions 4)

data ManagedRecord =
  ManagedRecord
  {

  } deriving (Show,Eq)


instance FromJSON ManagedRecord where
  parseJSON o = error (show ("managedrecord",o))



data DNSManagedSingle = DNSManagedSingle {
  dnsManagedSingleID :: RecordID
  } deriving (Show,Eq)

instance FromJSON DNSManagedSingle where
  parseJSON (Object o) = DNSManagedSingle <$> o .: "id"

instance ToHttpApiData RecordID where
  toUrlPiece = toUrlPiece . unRecordID


data RecordsFor =
  RecordsFor
  { rforTotalRecords :: Int
  , rforData         :: [DNSRecord]
  , rforPage         :: Int
  , rforTotalPages   :: Int
  }
  deriving (Generic, Eq, Show)



instance FromJSON RecordsFor where
  parseJSON = genericParseJSON (mkOptions 4)
