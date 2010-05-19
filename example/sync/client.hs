
import Network.BsonRPC
import Database.MongoDB.BSON
import Data.Int
import qualified Data.ByteString.Lazy.UTF8 as L8
import Control.Monad
import System.Environment (getArgs)

main = do 
  let msg = [(L8.fromString "test1", toBson (12345 :: Int64))]
  ports <-  liftM (map read) getArgs
  friends <- forM ports (connectPeer "127.0.0.1") 
    
  !results <- call msg (Faction friends)
  putStr "done. here are the responses: "
  print results

