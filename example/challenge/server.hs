
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
  putStrLn "Listening for connections"
  serve 12345 (ServiceCallback handleConnection, Nothing)

handleStep1 :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep1 doc = 
  let mdoc = fromBsonDoc doc
      user = fromBson $ fromJust $ M.lookup "username" mdoc
  in if user == "manuel" then do
    sc <- getStdRandom random :: IO Int
    let response = [("challenge", toBson sc)]
    return $ Just (toBsonDoc response, ServiceCallback (handleStep1 sc))
    else throwIO $ userError "unknown user" 

handleStep2 :: Int -> BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep2 sc doc =
  let mdoc = fromBsonDoc doc
      cc = fromBson $ fromJust $ M.lookup "challenge" mdoc
      (BsonBinary BSTUserDefined resp) = fromJust $ M.lookup "response" mdoc 
      chash = hash2LBS $ (w32_to_w8s (fromIntegral cc)) 
                   ++ (w32_to_w8s (fromIntegral sc)) 
                   ++ secret
  in return $ if chash == resp then 
    let shash = hash2LBS $ (w32_to_w8s (fromIntegral sc)) 
                  ++ (w32_to_w8s (fromIntegral cc)) 
                  ++ secret
        response = [("response", (BsonBinary BSTUserDefined shash))]
    in (putStrLn "User is authorized") >> return $ Just (toBsonDoc response, ServiceCallback mainHandler)

mainHandler :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
mainHandler doc = print doc >> return (nullMessage (ServiceCallback mainHandler))


