import Control.Parallel.Strategies

fib :: (Num a, Eq a) => a -> a
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1)+ fib (x - 2)

test = runEval $ do
  x <- rpar(fib(30))
  y <- rpar(fib(30))
  z <- rpar(fib(30))
  return (x, y, z)

main = print test
