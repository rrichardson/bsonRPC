
import Network.BsonRPC
import Database.MongoDB.BSON
import Data.Int
import qualified Data.ByteString.Lazy.UTF8 as L8
import Control.Monad
import Control.Concurrent
import System.Environment (getArgs)
import Maybe 

import Util

main = do 
  let msg = [(L8.fromString "count", toBson (1 :: Int))]
  max <- liftM (read . head) getArgs
  friend <- connectPeer "127.0.0.1" 12345
  block <- newEmptyMVar
  asyncCall msg (ServiceCallback (handleCall max block)) friend
  _ <- takeMVar block
  return () 

handleCall :: Int -> MVar () -> BsonDoc -> IO (Maybe (BsonDoc, ServiceCallback)) 
handleCall max block doc = 
  let cur = (fromBson . fromJust) (lookupl8 "count" doc)
  in if (cur >= max) 
    then putMVar block () >> return Nothing 
    else return $ Just ([(L8.fromString "count", toBson (cur + 1 ))], ServiceCallback (handleCall max block) )

