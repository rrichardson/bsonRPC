
import Network.BsonRPC
import Database.MongoDB.BSON
import qualified Data.Map as M
import Data.Int
import Control.Monad

main = do 
  let msg = M.fromAscList [("test1", toBson (9876543219876543 :: Int64)), 
                           ("test2", toBson (12345 :: Int32)), 
                           ("test3", toBson ("6789"))] 

  !friend <- connectPeer "127.0.0.1" 12345
  cast (toBsonDoc msg) friend
  resp <- call (toBsonDoc msg) friend
  putStrLn $ "got response: " ++ (show resp)
  forM_ resp (putStrLn . show)
  shutdown friend
  return ()

  
