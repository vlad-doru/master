import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Network
import System.IO
import System.IO.Error
import Text.Read
import Text.Printf

errorHandling :: IOError -> IO ()
errorHandling e = putStrLn "Closing client."

type Factor = TVar Integer
data Client = Client {handle::Handle chan::(TChan String)} deriving (Eq)

newClient :: Handle -> Client
newClient h = Client {handle = h, chan = newTChan}

newFactor :: Integer -> IO Factor
newFactor n = atomically $ do
  f <- newTVar n
  return f

changeFactor :: Factor -> Integer -> IO ()
changeFactor f n = do
  atomically $ do
    writeTVar f n
  putStrLn $ "Changed the factor to: " ++ (show n)

multiplyWithFactor :: Integer -> Factor -> IO Integer
multiplyWithFactor x f = atomically $ do
  n <- readTVar f
  return (x * n)

talk :: Client -> Factor -> IO ()
talk c f = do
  hSetBuffering h LineBuffering
  loop where
  loop = do
    let h = handle c
    l <- hGetLine h 

port = 8888 :: Integer

serveClient :: Socket -> Factor -> Clients -> IO ()
serveClient sock factor clients = do 
  (handle, host, port) <- accept sock
  let client = newClient handle
  addClient client clients
  printf "Accepted connection from %s:%s\n" host (show port)
  forkIO $ (talk client factor `finally` removeClient client clients) `catch` errorHandling 
  return ()

main = do 
  sock <- listenOn(PortNumber(fromIntegral port))
  factor <- newFactor 2
  clients <- newEmptyClientsList
  printf "Listening on port %d\n" port
  forever $ serveClient sock factor clients


  
