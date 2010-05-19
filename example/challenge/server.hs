
import qualified Data.ByteString.Lazy as L
import Control.Exception (throwIO)
import System.Random(getStdRandom,random)
import Maybe (fromJust)

import Network.BsonRPC
import Database.MongoDB.BSON
import Util

secret = (L.unpack . l8toS) "secret password"

main = do 
  putStrLn "Listening for connections"
  serve 12345 (Just (ServiceCallback handleStep1), Nothing)

handleStep1 :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep1 doc = 
  let user = fromBson $ fromJust $ lookupl8 "username" doc
  in if user == "manuel" then do
    putStrLn "got user, starting step 1"
    sc <- getStdRandom random :: IO Int
    let response = [(l8toS "challenge", toBson sc)]
    return $ Just (response, ServiceCallback (handleStep2 sc))
    else throwIO $ userError "unknown user" 

handleStep2 :: Int -> BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleStep2 sc doc =
  let cc = fromBson $ fromJust $ lookupl8 "challenge" doc
      (BsonBinary BSTUserDefined resp) = fromJust $ lookupl8 "response" doc 
      chash = hash2LBS $ w32tow8s (fromIntegral cc) 
                      ++ w32tow8s (fromIntegral sc) 
                      ++ secret
  in putStrLn "starting step 2" >>
    if chash == resp 
    then let shash = hash2LBS $ w32tow8s (fromIntegral sc) 
                             ++ w32tow8s (fromIntegral (cc :: Int) ) 
                             ++ secret
             response = [(l8toS "response", (BsonBinary BSTUserDefined shash))]
    in putStrLn "User is authorized" >> return (Just (response, ServiceCallback mainHandler))
    else throwIO $ userError "Invalid user supplied hash"

mainHandler :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
mainHandler doc = print doc >> return Nothing 


