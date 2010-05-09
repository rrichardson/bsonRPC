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


module Network.BsonRPC
(
  Connection, -- Generic Connection Interface
  Faction,    -- Group of Connections to peers, treated as a single, broadcasting connection
  Peer,       -- Single Connection
  connectPeer, -- Connect to a remote host
  connectPeerEx,  -- Connect to a remote host in ipv6 or ipv4
  connectFaction, -- Connect to a list of remote hosts and group them
  connectFactionEx, -- Connect to a list of remote hosts via ipv6 or ipv4 and group them
  call,        -- Synchronous Call with response
  cast,        -- Asynchronous call with no response
  asyncCall,   -- Asynchronous call with callbacks for responses
  serve,       -- Listen on a port and respond to requests with a callback
  serveEx      -- Listen on a port and ipv6 or ipv4 interface ... callback
) where

import Network.BsonRPC.Util
import Data.ByteString
import Database.MongoDB.BSON 
import Control.Concurrent.MVar
import Data.Int
import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe -- for debugging purposes 
import qualified Network.Socket as Sock

type ServiceCallback = BsonMessage -> IO BsonMessage

class Connection a where 
  -- | Synchronous call to a remote peer (or peers) which returns the reply
  call :: a -> BsonMessage -> IO [BsonMessage]

  -- | Asynchronous call to a remote peer (or peers) which does not wait 
  --   nor does it expect a reply
  cast :: a -> BsonMessage -> IO ()

  -- | Asynchronous call to a remote peer (or peers) which returns immediately
  --   but takes a callback for responses as they arrive
  asyncCall :: a -> BsonMessage -> (BsonMessage -> IO ()) -> IO ()

data Peer = Peer { pId :: ByteString,
                   pSvcId :: ByteString, 
                   pPort :: Int, 
                   pCurrent :: MVar Int64, 
                   pConn :: Handle 
                 } deriving (Show)

data Faction = Faction [Peer] deriving (Show)

instance Show (MVar Int64) where
  show a = Prelude.show $ unsafePerformIO $ readMVar a

{-
instance Connection Peer where
  call p msg = 
  cast p msg = 
  asyncCall p msg cb = 

instance Connection Faction where
  call p msg = 
  cast p msg = 
  asyncCall p msg cb = 
-}
{- 
 - Server Functions
 -}

serve :: Int -> ServiceCallback -> IO ()
serve port cb = serveEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) Sock.iNADDR_ANY) cb

serveEx :: Sock.Family -> Sock.SockAddr -> ServiceCallback -> IO () 
serveEx fam addr cb = do
  h <- listenOnEx fam addr
  return ()

{- 
 - Client Functions
 -}

connectPeer :: String -> Int -> IO (Peer)
connectPeer host port = do 
  addr <- Sock.inet_addr host
  connectPeerEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) addr)

connectPeerEx :: Sock.Family -> Sock.SockAddr -> IO (Peer)
connectPeerEx fam addr = do
  let p = case addr of 
            (Sock.SockAddrInet port _) -> port
            (Sock.SockAddrInet6 port _ _ _) -> port
            _ -> 0 
  h <- doWithNet fam addr (\s a -> Sock.connect s addr )
  createPeer h p 
    

initMessage :: Peer -> Int64 -> BsonDoc -> IO BsonMessage
initMessage peer respto doc = do
  mid <- getNextMsgId peer
  let hdr = BsonHeader 0 0 0 mid respto 0 0 0 0
  return $ BsonMessage hdr doc 

getNextMsgId :: Peer -> IO Int64
getNextMsgId peer = modifyMVar (pCurrent peer) (\x -> let y = x + 1 in return (y, y) ) 
 
createPeer :: Handle -> Sock.PortNumber -> IO Peer
createPeer h p = do
  c <- newMVar (0 :: Int64)
  return $ Peer B.empty B.empty (fromIntegral p) c h 
