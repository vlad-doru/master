import Control.Concurrent ( threadDelay ) 
import Data.Binary 
import Data.Typeable 
import Control.Monad (forever,mapM_) 
import Control.Distributed.Process  
import Control.Distributed.Process.Node 
import Network.Transport.TCP 


host = "localhost"
port = "8080"

client :: ProcessId -> Process ()
client serverPid = forever $ do
  send serverPid "Hello!"
  liftIO $ do 
    threadDelay $ 1 * 10^6
    return ()
  
server :: Process ()
server = forever $ do 
  message <- expect
  say $ "Received message: " ++ message

main = do 
  Right transport <- createTransport "localhost" "8080" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node $ do
    serverPid <- spawnLocal server
    spawnLocal (client serverPid)
    liftIO $ getLine
    return ()
  

