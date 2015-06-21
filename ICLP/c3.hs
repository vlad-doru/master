import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x 
  | x < 0 = error "Cannot compute negative fibonacci numbers."
  | x >= 2 = fib (x - 1) + fib (x - 2)

parfib numbers = parMap rseq id (map (\x -> fib x) numbers)

myParMap :: (NFData b) => (a -> b) -> [a] -> Eval [b]
myParMap _ [] = return []
myParMap f (x:xs) = do
  r <- rpar((f x))
  rs <- myParMap f xs
  return (r:rs)

myparfib numbers = runEval (myParMap fib numbers)

main = do
  putStrLn("Welcome")
  args <- getArgs
  let numbers = map (\arg -> (read arg)::Integer) args
  print(show (myparfib numbers))
