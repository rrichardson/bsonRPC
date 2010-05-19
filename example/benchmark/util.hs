module Util where

import Data.Digest.SHA2
import Codec.Utils (Octet)
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L8
import Data.List (lookup)

-- stolen from http://gregheartsfield.com/2007/09/16/haskell-hmac.html
w32tow8s :: Word32 -> [Octet]
w32tow8s a = [fromIntegral (shiftR a 24),
              fromIntegral (shiftR a 16),
              fromIntegral (shiftR a 8),
              fromIntegral a]     

hash2LBS :: (Bits a, Integral a) => [a] -> L.ByteString
hash2LBS = L.pack . toOctets . sha256

l8toS :: String -> L8.ByteString
l8toS = L8.fromString

lookupl8 :: String -> [(L.ByteString, b)] -> Maybe b 
lookupl8 = lookup . L8.fromString
