module Util where

import Data.Digest.SHA2
import Codec.Utils (Octet)
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L


-- stolen from http://gregheartsfield.com/2007/09/16/haskell-hmac.html
w32_to_w8s :: Word32 -> [Octet]
w32_to_w8s a = (fromIntegral (shiftR a 24)) :
               (fromIntegral (shiftR a 16)) :
               (fromIntegral (shiftR a 8)) :
               (fromIntegral a) : []     

hash2LBS :: (Bits a, Integral a) => [a] -> L.ByteString
hash2LBS = L.pack . toOctets . sha256

