-- Producer consumer cu un buffer ce este un canal de comunicatie. Producerul va avea 2 tipuri de produse.
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Random

data Product = A | B deriving (Eq)
type CountVar = MVar Int

seconds :: Int
seconds = 10 ^ 6

consumer :: (Chan Product) -> Integer -> IO((Int, Int))
consumer chan 0 = do
  return (0, 0)
consumer chan n = do 
  (x, y) <- consumer chan (n - 1)  
  p <- readChan chan
  if p == A 
  then return (x + 1, y)
  else return (x, y + 1)

producer :: (Chan Product) -> IO ()
producer chan = forever $ do
  x <- randomRIO (1, 2) :: IO Int 
  if x == 1
  then writeChan chan A
  else writeChan chan B
  threadDelay $ 1 * 10^5

main = do
  chan <- newChan
  forkIO $ producer chan
  (a, b) <- consumer chan 10 
  putStrLn $ "We consumed " ++ (show a) ++ " A products and " ++ (show b) ++ " B products."
  

