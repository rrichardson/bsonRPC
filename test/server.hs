import Network.BsonRPC
import Database.MongoDB.BSON
import qualified Data.Map as M
import Data.Int

main = do 
  serve 12345 (Just (ServiceCallback handleCast), Just (ServiceCallback handleCall))
 
handleCast :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleCast doc = putStrLn ("cast: " ++ (show doc)) >> return Nothing

handleCall :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleCall doc = do
  putStrLn ("call: " ++ (show doc))
  return $ Just (doc, ServiceCallback handleCall)


