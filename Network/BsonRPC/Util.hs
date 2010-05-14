{-

Copyright (C) 2010 Rick Richardson <rick.richardson@gmail.com> 

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-}


module Network.BsonRPC.Util
(
  listenOn,
  listenOnEx,
  doWithSocket,
  getMessage,
  putMessage,
  BsonMessage(..),
  BsonHeader(..),
  protoTCP
)
where

import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import System.IO
import Foreign.C.Types (CInt)
import Database.MongoDB.BSON
import Network.BsonRPC.BinUtil
import qualified Network.Socket as Sock
import qualified Data.ByteString.Lazy as L
import Control.Concurrent
import Control.Concurrent.QSem
import Control.Concurrent.Chan
import Control.Monad
import Control.Applicative

data BsonHeader = BsonHeader { bhSize        :: Int16,
                               bhType        :: Int16,
                               bhVersion     :: Int16,
                               bhDest        :: Int16,
                               bhMessageId   :: Int64,
                               bhResponseTo  :: Int64,
                               bhOriginator1 :: Word32,
                               bhOriginator2 :: Word32,
                               bhOriginator3 :: Word32, 
                               bhOriginator4 :: Word32
                              } deriving (Show) -- 40 bytes total
                             
data BsonMessage = BsonMessage BsonHeader BsonDoc deriving (Show) 



protoTCP   = (6 :: CInt)
protoUDP   = (17 :: CInt)
protoSCTP  = (132 :: CInt)

listenOn :: Int -> IO Sock.Socket
listenOn port = listenOnEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) Sock.iNADDR_ANY)

listenOnEx :: Sock.Family -> Sock.SockAddr -> IO Sock.Socket
listenOnEx fam addr = do
  let p = case addr of 
            (Sock.SockAddrInet port _) -> port
            (Sock.SockAddrInet6 port _ _ _) -> port
            _ -> 0 
  doWithSocket fam addr (\s a -> do 
                            Sock.bindSocket s a
                            Sock.listen s (fromIntegral p))

doWithSocket :: Sock.Family -> Sock.SockAddr -> (Sock.Socket -> Sock.SockAddr -> IO ()) -> IO Sock.Socket
doWithSocket fam addr fun = do
  s <- Sock.socket fam Sock.Stream protoTCP
  fun s addr
  return s

getMessage :: Handle -> IO (BsonMessage)
getMessage h = do
  hdrBytes <- L.hGet h 40
  let hdr = flip runGet hdrBytes $ 
              BsonHeader <$> getI16 <*> getI16 <*>
                getI16 <*> getI16 <*> getI64 <*> getI64 <*>
                getW32 <*> getW32 <*> getW32 <*> getW32
  docBytes <- L.hGet h ((fromIntegral (bhSize hdr))- 40)
  let doc = runGet getBsonDoc docBytes
  return $ BsonMessage hdr doc 

putMessage :: Handle -> BsonMessage -> IO ()
putMessage h (BsonMessage hdr doc) = do
  let msg = runPut $ putBsonDoc doc
  let str = runPut $ do
                putI16 $ fromIntegral (40 + L.length msg)
                putI16 $ bhType hdr 
                putI16 $ bhVersion hdr
                putI16 $ bhDest hdr
                putI64 $ bhMessageId hdr
                putI64 $ bhResponseTo  hdr
                putW32 $ bhOriginator1 hdr
                putW32 $ bhOriginator2 hdr
                putW32 $ bhOriginator3 hdr
                putW32 $ bhOriginator4 hdr
                putLazyByteString msg
  L.hPut h str
  hFlush h
