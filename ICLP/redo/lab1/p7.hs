primes = (sieve [2, 3 ..])
  where sieve (x:xs) = x: sieve[y | y <- xs, y `mod` x > 0]

main = do
  putStrLn . show $ take 20 primes
  
