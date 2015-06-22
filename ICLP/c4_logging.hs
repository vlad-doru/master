import Control.Concurrent
import Control.Monad


data LogMessage = Message String | Stop (MVar ())
data Logger = Logger (MVar LogMessage)

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO $ processLogs l
  return l

processLogs :: Logger -> IO ()
processLogs (Logger m) = loop where
  loop = do 
    message <- takeMVar m
    case message of 
      Message s -> do
        putStrLn s
        loop
      Stop s -> do
        putStrLn "Stopping the logger."
        putMVar s ()

stopLogger :: Logger -> IO ()
stopLogger (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
  return ()

logMessage :: Logger -> String -> IO()
logMessage (Logger m) s = do
  putMVar m (Message s)
  return ()

main = do
  l <- initLogger
  logMessage l "Ana are"
  logMessage l "cele mai bune mere."
  stopLogger l
