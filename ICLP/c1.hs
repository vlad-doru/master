fact :: (Num a, Ord a) => a -> a
fact x 
  | x < 0 = error "Cannot compute factorial for negative numbers"
  | x == 0 = 1
  | x > 0 = x * fact (x - 1)

-- Schimba 1 in 0 peste tot
schimb :: [Int] -> [Int]
schimb [] = []
schimb (x:xs) 
  | x == 1 = 0 : schimb xs
  | otherwise = x : schimb xs

-- quicksort
qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs [y | y <- xs, y <= x] ++ [x] ++ qs [y | y <- xs, y > x]
-- qs cu filter
qsf :: [Int] -> [Int]
qsf [] = []
qsf (x:xs) = qsf (filter (<= x) xs) ++ [x] ++ qsf (filter (> x) xs)
-- Fibonacci
fib = 0:1:[x + y | (x, y) <- zip fib (tail fib)]
