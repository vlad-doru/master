import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import System.Random

type Account = TVar Int

deposit :: Account -> Int -> STM ()
deposit acc ammount  = do         
  x <- readTVar acc
  writeTVar acc (x + ammount)

withdraw  :: Account -> Int -> STM ()
withdraw  acc ammount  = do         
  x <- readTVar acc                
  writeTVar acc (x - ammount)

limitedwithdraw  :: Account -> Int -> STM ()
limitedwithdraw  acc ammount  = do         
  x <- readTVar acc                
  if ammount > x
  then retry
  else writeTVar acc (x - ammount)
  

transfer :: Account -> Account -> Int -> IO()
transfer from to ammount = atomically  $ do 
  withdraw from ammount 
  deposit to ammount

extras :: Account -> String -> IO()
extras acc str =  do
  x <- atomically $ readTVar acc 
  putStrLn ("Contul " ++ str ++ ": " ++ (show x))

creeazaNConturi :: Int -> STM [Account]
creeazaNConturi 0 = return []
creeazaNConturi n = do 
  cs <- creeazaNConturi (n - 1)
  c <- newTVar 1000
  return (c:cs)


main = do 
  --aMVar <- atomically $ newTVar 1000       
  --bMVar <- atomically $ newTVar 1000       
  (a,b) <- atomically $ do
    a <- newTVar 1000
    b <- newTVar 1000
    return (a,b)
  forkIO(transfer a b 300)
  forkIO (transfer b a 500)
  threadDelay $ 1 * 10^4
  extras a "a"
  extras b "b"       
  cs <- atomically $ creeazaNConturi 10









