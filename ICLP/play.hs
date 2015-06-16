repl :: (Num n, Ord n) => n -> a -> [a]
repl 0 x = []
repl 1 x = [x]
repl n x = [x] ++ r
  where r = repl (n - 1) x


-- Take first n elements
takke :: (Num n, Ord n) => n -> [a] -> [a]
takke n _ 
  | n < 0 = error "Cannot takke last than 0 elements."
  | n == 0 = []
takke _ [] = []
takke n (x:xs) = [x] ++ takke (n-1) xs

-- Reverse a list
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- Quicksort implementation
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) =
  let lower = [l | l <- xs, l <= x]
      upper = [u | u <- xs, u > x]
  in quick lower ++ [x] ++ quick upper

-- ZipWith
zipw :: (a -> b -> c) -> [a] -> [b] -> [c]
zipw _ [] _ = []
zipw _ _ [] = []
zipw f (x:xs) (y:ys) = (f x y) : zipw f xs ys

-- Flipp
flipp :: (a -> b -> c) -> b -> a -> c
flipp f b a = f a b

-- Quicksort with filter
quikk :: (Ord a) => [a] -> [a]
quikk [] = []
quikk (x: xs) = quikk (filter (<= x) xs) ++ [x] ++ quikk (filter (>x) xs)

-- Check if element is part of a list
ellem :: (Eq a) => a -> [a] -> Bool 
ellem x l = foldl (\ a y -> a || (x == y) ) False l 
