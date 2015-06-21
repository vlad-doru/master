import Data.List

-- PROBLEMA 1 --
-- Ecuatia de gradul 2 

-- Afiseaza un string pe o linie si apoi intoarce un Double
readNumber :: String -> IO Double
readNumber message = do 
  putStr(message)
  s <- getLine
  let a = read s :: Double
  return a

-- We can have 1 or 2 solution depending on the value of delta. If delta is negative then error.
type Solution = Either (Double, Double) Double

solve :: Double -> Double -> Double -> Solution
solve a b c  
    | delta > 0 = Left (((-b) + sqrt delta) / (2 * a), ((-b) - sqrt delta) / (2 * a)) 
    | delta == 0 = Right ((-b) / (2 * a))
    | otherwise = error "Faulty value for delta."
    where delta = b^2 - 4*a*c 

eq2 = do
  a <- readNumber "Coeficientul lui x^2: "
  b <- readNumber "Coeficientul lui x: "
  c <- readNumber "Termenul liber: "  
  case solve a b c of
    Left (x0, x1) -> putStrLn("Solutiile sunt: " ++ (show x0) ++ " " ++ (show x1))
    Right x0 -> putStrLn("Solutia unica este: " ++ show x0)

-- PROBLEMA 2 --
-- Functia reverse pe liste

revList :: [a] -> [a]
revList [] = []
revList (x:xs) = revList xs ++ [x]

-- PROBLEMA 3 --
-- Scrieti o functie care verifica daca un sir e palindrom

isPali :: [Char] -> Bool 
isPali "" = True
isPali s 
  | length s == 1 = True
  | head s /= last s = False
  | otherwise = isPali . init . tail $ s

-- PROBLEMA 4 --
-- Scrieti un exemplu de folosire a functiei zip si scrieti zip3

-- Exemplu: facem dot product
dot :: (Num a) => [a] -> [a] -> a
dot xs ys 
  | length xs /= length ys = error "Vectorii au numar de componente diferite."
  | otherwise = sum $ map (\(x, y) -> x * y) (zip xs ys)  
-- Implementarea de la zip3
z3 :: [a] -> [b] -> [c] -> [(a, b, c)]
z3 (x:xs) (y:ys) (z:zs) = (x, y, z) : (z3 xs ys zs)
z3 [] _ _ = []
z3 _ [] _ = []
z3 _ _ [] = []

-- PROBLEMA 5 --
-- Scrieti in doua moduri o functie map2 care are ca argument o functie cu doua argumente f si doua liste [x1,x2 ], [y1,y2,..] si intoarce lista [f(x1,y1), f(x2,y2),..].

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f x y 
  | length x /= length y = error "Listele trebuie sa aiba aceeasi lungime."
  | length x == 0 && length y == 0 = []
  | otherwise = (f (head x) (head y)) : map2 f (tail x) (tail y)

-- PROBLEMA 6
-- Scrieti o functie fn care are ca argument o functie f si un  numar n iar ca rezultat fn(x)=f(f(..(f(x)))) (de n ori)
fn :: (a -> a) -> Int -> (a -> a)
fn f n 
  | n < 0 = error "n trebuie sa fie >= 0"
  | n == 0 = \x -> x
  | n > 0 = f . (fn f (n - 1))

-- PROBLEMA 7
-- Folosind ciurul lui Eratostene creati o lista infinita care contine numerele prime.

primes :: [Integer]
primes = 2:sieve [3,5..]
  where
      sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
