
import Network.BsonRPC
import Database.MongoDB.BSON
import qualified Data.Map as M
import Data.Int

main = do 
  let msg = M.fromAscList [("test1", toBson (9876543219876543 :: Int64)), 
                           ("test2", toBson (12345 :: Int64)), 
                           ("test3", toBson (6789 :: Int64))] 
  !friend <- connectPeer "127.0.0.1" 12345
  cast (toBsonDoc msg) friend
  
