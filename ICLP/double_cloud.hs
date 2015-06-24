{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
import Control.Concurrent ( threadDelay ) 
import Data.Binary 
import Data.Typeable 
import Control.Monad (forever,mapM_) 
import Control.Distributed.Process  
import Control.Distributed.Process.Node 
import Network.Transport.TCP 
import GHC.Generics (Generic)
import Data.Typeable
import Data.Binary


host = "localhost"
port = "8080"

data DoubleMsg = Double Int deriving (Typeable, Generic)
instance Binary DoubleMsg

type LogMsg = String 
type ReplyMsg = (ProcessId, String)

doubleInt :: DoubleMsg -> Process ()
doubleInt (Double x) = do 
  say $ "Dublul lui " ++ (show x) ++ " este: " ++ (show (2 * x))

logMsg :: LogMsg -> Process ()
logMsg x = do 
  say x

replyMsg :: ReplyMsg -> Process ()
replyMsg (pid, m) = do 
  say $ "Sending message to " ++ (show pid)
  send pid m
 
server :: Process ()
server = forever $ do 
  message <- expect
  say $ "Received message: " ++ message

main = do 
  Right transport <- createTransport "localhost" "8080" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node $ do
    serverPid <- spawnLocal $ forever $ do 
                              receiveWait [match doubleInt, match logMsg, match replyMsg] 
    spawnLocal $ do
      workerPid <- getSelfPid
      say "Send some messages."  
      send serverPid (Double 30)
      mapM_ (send serverPid) ["ana", "are"]
      send serverPid (workerPid, "r")
      send serverPid (Double 30)
      x <- expect
      say $ "Worker: " ++ x
    liftIO $ getLine 
    return ()

