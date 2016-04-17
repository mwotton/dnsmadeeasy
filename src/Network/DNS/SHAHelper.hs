module Network.DNS.SHAHelper where

import Crypto.HMAC
import Crypto.Hash.CryptoAPI
import Data.Hex(hex)
import qualified Data.Serialize  as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
-- import Codec.Utils (listToOctets)
-- import Codec.Utils (listFromOctets)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)

-- looks weird, but it produces the same result as the Ruby version.
genSHA1 :: String -> String -> String
genSHA1 key string = map toLower $ BS8.unpack $ hex $ C.encode $ (hmac (MacKey $ BS8.pack key) (BL8.pack string) :: SHA1)
