import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Random

type Restaurant = TVar Int
type RestCoada = QSem 

newRestaurant :: Int -> IO Restaurant 
newRestaurant size= atomically $ do
    r <- newTVar size
    return r

newRestCoada :: Int -> IO RestCoada
newRestCoada size = do
  r <- newQSem size
  return r

takeSeat :: Restaurant -> IO ()
takeSeat r = atomically $ do
  x <- readTVar r
  if x == 0 
  then retry
  else writeTVar r (x - 1)
  return ()

releaseSeat :: Restaurant -> IO ()
releaseSeat r = atomically $ do
  x <- readTVar r
  writeTVar r (x + 1)
  return ()

client :: Restaurant -> Int -> IO ()
client r index = do
  -- Take a seat
  takeSeat r
  putStrLn $ "Client " ++ (show index) ++ " started to eat."
  dineTime <- randomRIO(1, 5)
  threadDelay $ dineTime * (10 ^ 6)
  releaseSeat r
  putStrLn $ "Client " ++ (show index) ++ " finished eating."

clientCoada :: RestCoada -> Int -> IO ()
clientCoada r index = do 
  waitQSem r 
  putStrLn $ "Client " ++ (show index) ++ " started to eat."
  dineTime <- randomRIO(1, 5)
  threadDelay $ dineTime * (10 ^ 6)
  signalQSem r
  putStrLn $ "Client " ++ (show index) ++ " finished eating."

main = do
  print "Hello"
  r <- newRestCoada 10
  as <- mapM (async . (clientCoada r)) [1..20]
  mapM_ wait as
  print "Closing.."
