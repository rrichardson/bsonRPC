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
  ServiceCallback(..),
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

import Data.Int
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.IO.Unsafe -- for read-only debugging purposes 
import qualified Network.Socket as Sock
import Network.BsonRPC.Util
import Database.MongoDB.BSON 
import qualified Data.ByteString as B


class Connection a where 
  -- | Synchronous call to a remote peer (or peers) which returns the reply
  call :: BsonDoc -> a -> IO [BsonDoc]

  -- | Asynchronous call to a remote peer (or peers) which does not wait 
  --   nor does it expect a reply
  cast :: BsonDoc -> a -> IO ()

  -- | Asynchronous call to a remote peer (or peers) which returns immediately
  --   but takes a callback for responses as they arrive
  asyncCall :: BsonDoc -> ServiceCallback -> a -> IO ()

data Peer = Peer { pId :: B.ByteString,
                   pSvcId :: B.ByteString, 
                   pPort :: Int, 
                   pCurrent :: MVar Int64, 
                   pConn :: Handle,
                   pPending :: MVar (M.Map Int64 ServiceCallback),
                   pListener :: ThreadId
                 } deriving (Show)

data Faction = Faction [Peer] deriving (Show)

data ServiceCallback = ServiceCallback (BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback)))

instance Connection Peer where
  call doc p = syncRequests doc [p]

  cast doc p = do
    msg <- initMessage p MsgTypeCast 0 doc
    putMessage (pConn p) msg

  asyncCall doc cb p = do 
    mid <- getNextMsgId (pCurrent p)
    addCallback (pPending p) mid cb
    msg <- initMessage p MsgTypeCall 0 doc
    putMessage (pConn p) msg 

instance Connection Faction where
  call doc (Faction prs) = syncRequests doc prs
     
  cast doc (Faction prs) = 
    forM_ prs $ \p -> cast doc p
    
  asyncCall doc cb (Faction prs) = 
    forM_ prs $ \p -> asyncCall doc cb p

{- 
 - Server Functions
 -}

serve :: Int -> (Maybe ServiceCallback, Maybe ServiceCallback) -> IO ()
serve port cbs = serveEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) Sock.iNADDR_ANY) cbs

serveEx :: Sock.Family -> Sock.SockAddr -> (Maybe ServiceCallback, Maybe ServiceCallback) -> IO () 
serveEx fam addr (callcb, castcb) = do
  s <- listenOnEx fam addr
  forever $ do 
    (rs, _) <- Sock.accept s
    rh <- Sock.socketToHandle rs ReadWriteMode
    --hSetBuffering rh NoBuffering
    forkIO $ handlePeer rh callcb castcb

handlePeer :: Handle -> Maybe ServiceCallback -> Maybe ServiceCallback -> IO ()
handlePeer h callcb castcb = do
  putStrLn "Client Connected"
  !self <- myThreadId >>= createPeer h 0
  forever $ do
    putStrLn "Before receiving message"
    (BsonMessage hdr doc) <- getMessage h
    putStrLn "Received Message"
    case (getMsgType hdr) of
      MsgTypeCall -> case callcb of 
        Nothing -> putStrLn "Received a call msg for which I don't have a handler"
        Just (ServiceCallback cb) -> do
          mrep <- cb doc
          case mrep of
            Nothing -> return ()
            Just (reply, newcb) -> do
              putStrLn "preparing to send reply"
              mid <- getNextMsgId (pCurrent self)
              msg <- initMessage self MsgTypeCall (bhMessageId hdr) reply
              addCallback  (pPending self) mid newcb
              putStrLn $ "sending reply: " ++ (show msg)
              putMessage h msg 
      MsgTypeCast -> case castcb of
        Nothing -> putStrLn "Received msg for which I don't have a handler"
        Just (ServiceCallback cb) -> do 
        cb doc
        return ()

{- 
 - Client Functions
 -}

connectPeer :: String -> Int -> IO Peer
connectPeer host port = do 
  addr <- Sock.inet_addr host
  connectPeerEx Sock.AF_INET (Sock.SockAddrInet (fromIntegral port) addr)

connectPeerEx :: Sock.Family -> Sock.SockAddr -> IO (Peer)
connectPeerEx fam addr = do
  let p = addrToIntPort addr
  sock <- doWithSocket fam addr (\s a -> Sock.connect s a )
  h <- Sock.socketToHandle sock ReadWriteMode
  --hSetBuffering h NoBuffering
  createListeningPeer h $ fromIntegral p 
 
connectFaction :: [(String, Int)] -> IO Faction
connectFaction xs = Faction `liftM` (forM xs con) 
  where con (h, p) = connectPeer h p

connectFactionEx :: Sock.Family -> [Sock.SockAddr] -> IO Faction
connectFactionEx fam addrs = Faction `liftM` (forM addrs con) 
  where con a = connectPeerEx fam a

  
{-
 - Private Functions
 -} 


listen :: Handle -> MVar (M.Map Int64 ServiceCallback) -> MVar Int64 -> IO ()
listen h m cur= do
  putStrLn "Listening for message"
  (BsonMessage hdr doc) <- getMessage h
  let mid = bhMessageId hdr
  cbmap <- readMVar m 
  case M.lookup mid cbmap of 
    Nothing -> return ()
    Just (ServiceCallback cb) -> do 
      modifyMVar m (\x -> let y = M.delete mid x in return (y, ())) -- get rid of the existing cb
      mres <- cb doc
      case mres of
        Nothing -> return ()
        Just (reply, newcb) -> do 
          newid <- getNextMsgId cur
          let newhdr = BsonHeader 0 (msgTypeToInt MsgTypeCall) 0 0 newid mid 0 0 0 0
          addCallback m newid newcb 
          putMessage h (BsonMessage newhdr reply)
    
 
syncRequests :: BsonDoc -> [Peer] -> IO [BsonDoc]
syncRequests doc prs = do  
    sem <- newQSem (length prs)
    c <- newChan
    forM_ prs $ \p -> do
      mid <- getNextMsgId (pCurrent p)
      msg <- initMessage p MsgTypeCall 0 doc
      registerSync p c sem mid
      putMessage (pConn p) msg
    waitQSem sem
    putStrLn $ "collecting " ++ (show (length prs))
    collect (length prs) c []
    where collect 0      _   acc = return acc
          collect count chan acc = do !i <- readChan chan; putStrLn (show i); collect (count - 1) chan (i:acc)

registerSync :: Peer -> Chan BsonDoc -> QSem -> Int64 -> IO ()
registerSync p chan sem mid = do
  let cb = ServiceCallback (\doc -> writeChan chan doc >> signalQSem sem >> return Nothing)
  addCallback (pPending p) mid cb

initMessage :: Peer -> MessageType -> Int64 -> BsonDoc -> IO BsonMessage
initMessage peer mtype respto doc = do
  mid <- getNextMsgId (pCurrent peer)
  let hdr = BsonHeader 0 (msgTypeToInt mtype) 0 0 mid respto 0 0 0 0
  return $ BsonMessage hdr doc 

getNextMsgId :: MVar Int64 -> IO Int64
getNextMsgId current = modifyMVar current (\x -> let y = x + 1 in return (y, y) ) 
 
createListeningPeer :: Handle -> Sock.PortNumber -> IO Peer
createListeningPeer h p = do
  c <- newMVar (0 :: Int64)
  cbm <- newMVar M.empty
  t <- forkIO $ forever $ listen h cbm c
  return $ Peer B.empty B.empty (fromIntegral p) c h cbm t

createPeer :: Handle -> Sock.PortNumber -> ThreadId -> IO Peer
createPeer h p t = do
  c <- newMVar (0 :: Int64)
  cbm <- newMVar M.empty
  return $ Peer B.empty B.empty (fromIntegral p) c h cbm t
 
addCallback :: MVar (M.Map Int64 ServiceCallback) -> Int64 -> ServiceCallback -> IO ()
addCallback pending mid cb = modifyMVar pending (\x -> let y = M.insert mid cb x in return (y, ()))

instance Show (MVar Int64) where
  show a = Prelude.show $ unsafePerformIO $ readMVar a

instance Show (MVar (M.Map Int64 ServiceCallback)) where
  show a = unsafePerformIO $ isEmptyMVar a >>= \b -> 
    return $ if b then "<empty>" else "<map full of functions>" 

addrToIntPort :: Sock.SockAddr -> Int
addrToIntPort (Sock.SockAddrInet port _)      = fromIntegral port
addrToIntPort (Sock.SockAddrInet6 port _ _ _) = fromIntegral port
addrToIntPort  _                              = 0

data MessageType = MsgTypeCall | MsgTypeCast

msgTypeToInt :: MessageType -> Int16
msgTypeToInt MsgTypeCall = 0
msgTypeToInt MsgTypeCast = 1

intToMsgType :: Int16 -> MessageType
intToMsgType 0 = MsgTypeCall
intToMsgType 1 = MsgTypeCast
intToMsgType _ = error "wtf?"

getMsgType :: BsonHeader -> MessageType
getMsgType = intToMsgType . bhType 
