import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent.STM
import Text.Printf
import System.Random

type Account = TVar Int
data Transaction = Transaction Account Account Int

deposit :: Account -> Int -> STM ()
deposit acc ammount  = do         
  x <- readTVar acc
  writeTVar acc (x + ammount)

withdraw  :: Account -> Int -> STM ()
withdraw  acc ammount  = do         
  x <- readTVar acc                
  if ammount > x
  then retry
  else writeTVar acc (x - ammount)

  
transfer :: Account -> Account -> Int -> STM()
transfer from to ammount = do 
  withdraw from ammount 
  deposit to ammount

extras :: Account -> String -> IO()
extras acc str =  do
  x <- atomically $ readTVar acc 
  putStrLn ("Contul " ++ str ++ ": " ++ (show x))

newAccount :: Int -> IO Account
newAccount value = atomically $ do
    a <- newTVar value 
    return a 

newRandomAccount :: Int -> IO Account
newRandomAccount _ = do
  value <- randomRIO(1000, 2000)
  account <- newAccount value
  return account

asyncTransfer :: Int -> Transaction -> IO (Async ())
asyncTransfer transactionID (Transaction from to value) = do
  a <- async $ do 
    atomically $ transfer from to value
    printf "Transaction %d completed.\n" transactionID 
  return a

generateAccounts :: Int -> IO [Account] 
generateAccounts n = do
  accounts <- mapM newRandomAccount [1..n]
  return accounts



main = do 
  accounts <- generateAccounts 100
  -- Generate 50 random values for transactions
  values <- mapM (\i -> randomRIO(10, 20)) [1..50]
  -- Generate the transactions
  let pairs = [(a, b) | a <- accounts, b <- accounts, a /= b]
  let transactions = map (\((a, b), value) -> Transaction a b value) (zip pairs values)
  as <- mapM (uncurry asyncTransfer) (zip [1..50] transactions)
  mapM wait as
  print "Finished"
