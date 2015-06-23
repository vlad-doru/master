import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

type BChan a = (Chan a,  QSem) 

newBChan :: Int -> IO (BChan a)
newBChan size = do
  c <- newChan 
  q <- newQSem size
  return (c, q)

writeBChan :: BChan a -> a -> IO ()
writeBChan (c, q) m = do
  waitQSem q
  writeChan c m
  return ()

readBChan :: BChan a -> IO a
readBChan (c, q) = do
  x <- readChan c
  signalQSem q
  return x

w :: (BChan String) -> IO ()
w c = do
  putStrLn "WAIT"
  writeBChan c "bune"
  putStrLn "WROTE BUNE "
  return ()

main = do
  print "Hello"
  c <- newBChan 2 
  writeBChan c "ana"
  writeBChan c "mere"
  async $ w c 
  threadDelay $ 1 * 10^6
  x <- readBChan c
  putStrLn x
  y <- readBChan c
  putStrLn y
  z <- readBChan c
  putStrLn z

