
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L8
import Control.Exception (throwIO)
import System.Random(getStdRandom,random)
import Maybe (fromJust)

import Network.BsonRPC
import Database.MongoDB.BSON
import Util

secret = (L.unpack . L8.fromString) "secret password"

main = do 
  let msg = [("auth", toBson (1 :: Int)), ("username", toBson "manuel")]
  putStrLn "Connecting to server"
  conn <- connectPeer "127.0.0.1" 12345
  asyncCall (toBsonDoc msg) (ServiceCallback handleStep1) conn
  shutdown conn

handleStep1 :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep1 doc =
  let mdoc = fromBsonDoc doc
      sc = fromBson $ fromJust $ M.lookup "challenge" mdoc
  in do
    cc <- getStdRandom random :: IO Int
    let hash = hash2LBS $ (w32_to_w8s (fromIntegral cc)) 
                 ++ (w32_to_w8s (fromIntegral sc)) 
                 ++ secret
    let response = [("response", (BsonBinary BSTUserDefined hash)), ("challenge",  toBson cc)]
    return $ Just (toBsonDoc response, ServiceCallback (handleStep2 sc cc))

handleStep2 :: Int -> Int -> BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep2 sc cc doc = 
  let mdoc = (fromBsonDoc doc)
      (BsonBinary BSTUserDefined sr) = fromJust $ M.lookup "response" mdoc
      hash = hash2LBS $ (w32_to_w8s (fromIntegral sc)) 
               ++ (w32_to_w8s (fromIntegral cc)) 
               ++ secret
  in if sr == hash 
    then return $ Just (toBsonDoc [("ok", toBson (1 :: Int))], ServiceCallback mainHandler)
    else throwIO $ userError "server hash was not correct"

mainHandler :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
mainHandler doc = print doc >> return (nullMessage (ServiceCallback mainHandler))

