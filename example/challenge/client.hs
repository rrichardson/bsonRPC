
import qualified Data.ByteString.Lazy as L
import Control.Exception (throwIO)
import System.Random(getStdRandom,random)
import Maybe (fromJust)
import System.IO (hGetChar, stdin)
import Network.BsonRPC
import Database.MongoDB.BSON
import Util

secret = (L.unpack . l8toS) "secret password"

main = do 
  let msg = [("auth", toBson (1 :: Int)), ("username", toBson "manuel")]
  putStrLn "Connecting to server"
  conn <- connectPeer "127.0.0.1" 12345
  asyncCall (toBsonDoc msg) (ServiceCallback handleStep1) conn
  _ <- getChar
  shutdown conn

handleStep1 :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep1 doc =
  let sc = fromBson $ fromJust $ lookupl8 "challenge" doc
  in do
    putStrLn "starting step 1"
    cc <- getStdRandom random :: IO Int
    let hash = hash2LBS $ w32tow8s (fromIntegral cc) 
                       ++ w32tow8s (fromIntegral sc) 
                       ++ secret
    let response = [( l8toS "response", (BsonBinary BSTUserDefined hash)), (l8toS "challenge",  toBson cc)]
    return $ Just (response, ServiceCallback (handleStep2 sc cc))

handleStep2 :: Int -> Int -> BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep2 sc cc doc = 
  let (BsonBinary BSTUserDefined sr) = fromJust $ lookupl8 "response" doc
      hash = hash2LBS $ w32tow8s (fromIntegral sc) 
                     ++ w32tow8s (fromIntegral cc) 
                     ++ secret
  in putStrLn "starting step 2" >>
    if sr == hash  
    then return $ Just ([(l8toS "ok", toBson (1 :: Int))], ServiceCallback mainHandler)
    else throwIO $ userError "server hash was not correct"

mainHandler :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
mainHandler doc = print doc >> return Nothing 

