import Control.Concurrent
import Control.Monad
import System.Random
import Text.Printf

data Product = A | B deriving (Eq)

producer :: (Chan Product) -> IO ()
producer c = forever $ do
  produceTime <- randomRIO(1, 5) 
  threadDelay $ produceTime * (10^5) 
  productType <- randomRIO(1, 2) :: IO Int
  let product = if productType `mod` 2 == 0 then A else B
  writeChan c product

consumer :: (Chan Product) -> Int -> IO (Int, Int)
consumer _ 0 = return (0, 0)
consumer c n = do
  (a, b) <- consumer c (n - 1)
  printf "Consumed %d A and %d B\n" a b
  x <- readChan c 
  consumeTime <- randomRIO(1, 5) 
  threadDelay $ consumeTime * (10^5)
  if x == A
  then return (a + 1, b)
  else return (a, b + 1)

main = do
  c <- newChan 
  forkIO $ producer c
  (a, b) <- consumer c 10
  printf "Consumed %d A and %d B" a b
