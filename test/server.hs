import Network.BsonRPC
import Database.MongoDB.BSON
import qualified Data.Map as M
import Data.Int

main = do 
  serve 12345 (\doc -> putStr (show doc) >> return Nothing)
    

