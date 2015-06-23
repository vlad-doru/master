import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.List.Split
import Data.Maybe
import Text.Read
import System.Environment

fib :: Int -> Int
fib n 
  | n == 0 = 1
  | n == 1 = 1
  | n > 1 = (fib $ n - 1) + (fib $ n - 2)
  | otherwise = error "The Fibonacci index must be positive."

solvePartition :: [Int] -> IO ([Int])
solvePartition l = do
  let r = map fib l
  return r

solveFib :: Int -> [Int] -> IO ()
solveFib n l = do
  let ls = chunksOf ((length l) `quot` n) l
  -- Start each partition asynchronously 
  as <- mapM (\l -> async $ solvePartition l) ls
  -- Wait for all partitions to be solved
  solutions <- mapM wait as
  mapM_ (\s -> putStrLn (show s)) (zip l (concat solutions))
  return ()

main = do
  [parts, file] <- getArgs  
  let parsed = readMaybe parts :: (Maybe Int)
  let n = if parsed == Nothing then error "Could not parse the number of partitions." else fromJust parsed
  contents <- readFile(file)
  let numbers = map (\l -> read l :: Int) (lines contents)
  solveFib n numbers
