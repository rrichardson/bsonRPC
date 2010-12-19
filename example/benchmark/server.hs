import Network.BsonRPC
import Data.Bson
import qualified Data.Map as M
import Data.Int
--import Data.UString
import System.Environment (getArgs)
import System.Random (getStdRandom, randomR)
import System.Posix.Unistd (sleep)

main = do
  args <- getArgs
  let port = (read . head) args 
  serve port (Just (ServiceCallback handleCall), Nothing)
 
handleCall :: Document -> IO (Maybe (Document, ServiceCallback))
handleCall doc = do
  --putStrLn ("call: " ++ (show doc))
  return $ Just (doc, ServiceCallback handleCall)


