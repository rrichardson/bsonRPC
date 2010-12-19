
import Network.BsonRPC
import Data.Bson
import Data.Int
import Data.UString
import Control.Monad
import Control.Concurrent
import System.Environment (getArgs)
import Maybe 

import Util

main = do 
  let msg = [(u "count", toBson (1 :: Int))]
  max <- liftM (read . head) getArgs
  friend <- connectPeer "127.0.0.1" 12345
  block <- newEmptyMVar
  asyncCall msg (ServiceCallback (handleCall max block)) friend
  _ <- takeMVar block
  return () 

handleCall :: Int -> MVar () -> Document -> IO (Maybe (BsonDoc, ServiceCallback)) 
handleCall max block doc = 
  let cur = (fromBson . fromJust) (lookupl8 "count" doc)
  in if (cur >= max) 
    then putMVar block () >> return Nothing 
    else return $ Just ([(L8.fromString "count", toBson (cur + 1 ))], ServiceCallback (handleCall max block) )

