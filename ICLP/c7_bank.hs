import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

-- Will have deposit, withdraw, transfer and showBalance.
type Account = TVar Integer

newAccount :: Integer -> IO Account
newAccount amount = atomically $ do
  t <- newTVar amount 
  return t

deposit :: Account -> Integer -> STM ()
deposit a value = do
  balance <- readTVar a 
  writeTVar a (balance + value)

withdraw :: Account -> Integer -> STM ()
withdraw a value = do
  balance <- readTVar a
  writeTVar a (balance - value)

showBalance :: Account -> IO()
showBalance a = do
  balance <- atomically $ do
    readTVar a
  putStrLn $ "Balance: " ++ (show balance)

transfer :: Account -> Account -> Integer -> IO ()
transfer a b value = 
  atomically $ do
    withdraw a value
    deposit b value

main = do
  a <- newAccount 1000
  b <- newAccount 1000
  forkIO $ transfer a b 300
  forkIO $ transfer b a 500
  threadDelay $ 10^6
  showBalance a
  showBalance b

