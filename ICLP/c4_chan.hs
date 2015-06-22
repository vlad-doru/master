import Data.Char
import Control.Monad
import Control.Concurrent

master :: (Chan String) -> IO ()
master cin = forever $ do
  line <- getLine
  writeChan cin line

splitWorker :: (Chan String) -> (Chan String) -> IO ()
splitWorker cin cout = forever $ do
  line <- readChan cin
  mapM_ (\w -> writeChan cout w ) (words line)

upperWorker :: (Chan String) -> IO ()
upperWorker cout = forever $ do
  word <- readChan cout
  putStrLn (map toUpper word)

main = do
  cin <- newChan
  cout <- newChan
  forkIO $ splitWorker cin cout -- a worker splits from cin and puts to cout
  forkIO $ upperWorker cout -- a worker calls to upper and prints
  master cin -- read messages and puts them to cin
