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
  doWithNet,
  getMessage,
  putMessage,
  BsonMessage(..),
  BsonHeader(..),
  protoTCP
)
where
import qualified Network.Socket as Sock
import qualified Data.ByteString.Lazy as L
import System.IO
import Foreign.C.Types (CInt)
import Database.MongoDB.BSON
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import Network.BsonRPC.BinUtil

data BsonHeader = BsonHeader { bhSize        :: Int32,
                               bhVersion     :: Int16,
                               bhDest        :: Int16, 
                               bhMessageId   :: Int64,
                               bhResponseTo  :: Int64,
                               bhOriginator1 :: Word32,
                               bhOriginator2 :: Word32,
                               bhOriginator3 :: Word32, 
                               bhOriginator4 :: Word32
                              } -- 40 bytes total
                             
data BsonMessage = BsonMessage BsonHeader BsonDoc 


protoTCP   = (6 :: CInt)
protoUDP   = (17 :: CInt)
protoSCTP  = (132 :: CInt)

listenOn :: Int -> IO Handle
listenOn port = listenOnEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) Sock.iNADDR_ANY)

listenOnEx :: Sock.Family -> Sock.SockAddr -> IO Handle
listenOnEx fam addr = do
  let p = case addr of 
            (Sock.SockAddrInet port _) -> port
            (Sock.SockAddrInet6 port _ _ _) -> port
            _ -> 0 
  doWithNet fam addr (\s a -> do 
                            Sock.bindSocket s addr
                            Sock.listen s (fromIntegral p))

doWithNet :: Sock.Family -> Sock.SockAddr -> (Sock.Socket -> Sock.SockAddr -> IO ()) -> IO Handle
doWithNet fam addr fun = do
  s <- Sock.socket fam Sock.Stream protoTCP
  fun s addr
  h <- Sock.socketToHandle s ReadWriteMode
  hSetBuffering h NoBuffering
  return h

getMessage :: Handle -> IO (BsonMessage)
getMessage h = do
  hdrBytes <- L.hGet h 44 
  return $ flip runGet hdrBytes $ do
                msgLen <- getI32
                ver    <- getI16
                dest   <- getI16
                mid    <- getI64
                respTo <- getI64
                origin1 <- getW32
                origin2 <- getW32
                origin3 <- getW32
                origin4 <- getW32
                doc    <- getBsonDoc
                return $ BsonMessage (BsonHeader msgLen ver dest mid respTo origin1 origin2 origin3 origin4) doc 

putMessage :: Handle -> BsonMessage -> Int64 -> IO ()
putMessage h (BsonMessage hdr doc) mid = do
  let msg = runPut $ putBsonDoc doc
  let hdstr = runPut $ do
                putI32 $ fromIntegral (44 + L.length msg)
                putI16 $ bhVersion hdr
                putI16 $ bhDest hdr
                putI64 $ bhMessageId hdr
                putI64 $ mid 
                putW32 $ bhOriginator1 hdr
                putW32 $ bhOriginator2 hdr
                putW32 $ bhOriginator3 hdr
                putW32 $ bhOriginator4 hdr
  L.hPut h $ hdstr `L.append` msg
