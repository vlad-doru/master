import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (catch, finally)
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Network
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Map as Map
import Data.Maybe
import Network
import System.Environment
import System.IO
import System.IO.Error
import Text.Printf

type BroadcastChan = Chan String
data Server = Server {clients::TVar (Map.Map String Client), 
                      sock::Socket, 
                      broadcast::BroadcastChan} deriving (Eq)
data Client = Client {name::String, 
                      handle::Handle, 
                      kicked::(TVar Bool),
                      bchan::BroadcastChan,
                      server::Server} deriving (Eq)

newClient :: Handle -> Server -> IO Client
newClient handle server = do
  hPrint handle "What is your name?"
  name <- hGetLine handle
  bc <- dupChan $ broadcast server
  kicked <- atomically $ newTVar False
  writeChan bc (printf "***%s is now connected" name)
  let client = Client {name=name, handle=handle, bchan = bc, server=server, kicked=kicked}
  atomically $ do
    let var = clients server
    m <- readTVar var
    let m' = Map.insert name client m
    writeTVar var m'
    return ()
  return $ client

closeClient :: Client -> Server -> IO ()
closeClient client server = do
  atomically $ do
    let var = clients server
    m <- readTVar var
    let m' = Map.delete (name client) m
    writeTVar var m'
  hClose (handle client)
  writeChan (broadcast server) (printf "***%s is now offline." (name client))

errorHandler :: Client -> Server -> IOError -> IO ()
errorHandler client server e = do
  atomically $ do
    let var = clients server
    m <- readTVar var
    let m' = Map.delete (name client) m
    writeTVar var m'
  writeChan (bchan client) (printf "***%s is disconnected." (name client))


kick :: Server -> String -> IO ()
kick server name = do
  client <- atomically $ do
    m <- readTVar (clients server)
    let c = Map.lookup name m
    return c
  if client == Nothing 
  then writeChan (broadcast server) (printf "<ERROR>: Client %s is not on the server." name)
  else do
    atomically $ do writeTVar (kicked (fromJust client)) True
    writeChan (broadcast server) (printf "<INFO>: %s was kicked." name)
  return ()

tell :: Server -> Client -> String -> String -> IO ()
tell server sender receiver message = do
  client <- atomically $ do
    m <- readTVar (clients server)
    let c = Map.lookup receiver m
    return c
  if client == Nothing 
  then hPrintf (handle sender) "<ERROR>: Client %s is not on the server.\n" receiver
  else do
    hPrintf (handle $ fromJust client) "<FROM %s>: %s\n" (name sender) message
  return ()

readCommand :: Client -> IO ()
readCommand c = do
  s <- hGetLine $ handle c
  let command = words s
  case command of
    ["/quit"] -> return ()
    ["/kick", name] -> do 
      kick (server c) name
      readCommand c
    "/tell":name:message -> do
      tell (server c) c name (unwords message)
      readCommand c
    _ -> do
      writeChan (bchan c) (printf "<%s>: %s" (name c) (unwords command))
      readCommand c

serveClient :: Client -> IO ()
serveClient c =  do 
  message <- readChan (bchan c)
  kicked <- atomically $ readTVar (kicked c)
  if kicked 
  then return ()
  else do 
    hPrint (handle c) message
    serveClient c

handleClients :: Server -> IO ()
handleClients server= forever $ do 
  (handle, host, port) <- accept $ sock server 
  printf "Accepted connection from %s:%s\n" host (show port)
  forkIO $ do 
    client <- newClient handle server
    (race_ (readCommand client) (serveClient client) `finally` closeClient client server) `catch` (errorHandler client server)
    return ()

main = do 
  [p] <- getArgs
  let port = read p :: Int 
  sock <- listenOn(PortNumber(fromIntegral port))
  printf "Listening on port %d\n" port
  broadcast <- newChan
  clients <- atomically $ newTVar Map.empty
  let server = Server {clients=clients, sock=sock, broadcast=broadcast}
  handleClients server

