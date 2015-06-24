import Control.Concurrent
import Control.Monad

type BChan a = (Chan a,  QSem) 

newBChan :: Int -> IO (BChan a)
newBChan size = do
  q <- newQSem size
  c <- newChan 
  return (c, q)

readBChan :: (Show a) => (BChan a) -> IO a
readBChan (c, q) = do
  m <- readChan c
  signalQSem q
  -- Just to see if this works accordingly
  print $ "Read " ++ (show m)
  threadDelay $ 5 * (10^5)
  return m 

writeBChan :: (Show a) => (BChan a) -> a -> IO()
writeBChan (c, q) m = do
  waitQSem q
  writeChan c m
  print $ "Wrote " ++ (show m)

main = do
  bc <- newBChan 10
  mapM_ (\i -> forkIO $ writeBChan bc (show i)) [1..20]
  r <- mapM (\i -> readBChan bc) [1..20]
  print $ show r




