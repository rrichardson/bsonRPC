import Network.BsonRPC
import Database.MongoDB.BSON
import qualified Data.Map as M
import Data.Int
import System.Environment (getArgs)
import System.Random (getStdRandom, randomR)
import System.Posix.Unistd (sleep)
main = do
  args <- getArgs
  let port = (read . head) args 
  serve port (Just (ServiceCallback handleCall), Nothing)
 
handleCall :: BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback))
handleCall doc = do
  secs <- getStdRandom $ randomR (1,5)
  putStrLn "thinking..."
  sleep secs
  putStrLn ("call: " ++ (show doc))
  return $ Just (doc, ServiceCallback handleCall)


