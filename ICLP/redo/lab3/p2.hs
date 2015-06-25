import Control.Concurrent.Async
import Control.Concurrent
import Control.Parallel.Strategies
import Control.Monad
import Data.Maybe
import Data.List.Split
import System.Environment
import Text.Read

fib :: Int -> Int 
fib x 
  | x == 0 = 1
  | x == 1 = 1
  | x > 1 = (fib (x -1)) + (fib (x - 2))
  | otherwise = error "Integer must be greater than 0."

solvePartition :: [Int] -> IO ([Int])
solvePartition l = do
  let r = map fib l
  return r

solveFile :: [String] -> Int -> IO [(Int, Int)]
solveFile ls np = do
  let ns = map (\x -> read x :: Int) ls
  let parts = chunksOf ((length ns) `quot` np) ns
  as <- mapM (\p -> async $ solvePartition p) parts
  tasks <- mapM wait as
  let solutions = concat tasks
  return $ zip ns solutions

solvePar :: [String] -> Int -> [(Int, Int)]
solvePar ls n = runEval $ do
  let parts = chunksOf ((length ls) `quot` n) (map (\x -> read x :: Int) ls)
  sparks <- mapM (\p -> rpar $ map fib p ) parts 
  tasks <- mapM rseq sparks
  let solutions = concat tasks
  return $ zip (concat parts) solutions



main = do
  [file, arg] <- getArgs 
  let n = readMaybe arg :: Maybe Int 
  if n == Nothing || fromJust n <= 0
  then do
    putStrLn "Numarul de partitii e invalid"
  else do
    let partitions = fromJust n
    contents <- readFile(file)
    let ls = lines contents
    {-solutions <- solveFile ls partitions-}
    let solutions = solvePar ls partitions
    {-mapM_ (\s -> print s) solutions-}
    {-print $ "Computed using " ++ (show partitions) ++ " partitions."-}
    print "Done"



