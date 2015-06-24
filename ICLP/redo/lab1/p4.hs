-- Folosire zip + zip3

dotProduct :: (Num a) => [a] -> [a] -> [a]
dotProduct x y 
  | length x == length y = map (\(m, n) -> n * m) (zip x y) 
  | otherwise = error "Vectorii trebuie sa aiba aceeasi dimensiune"

z3 :: [a] -> [a] -> [a] -> [(a, a, a)]
z3 xs ys zs 
  | (length xs == 0) || (length ys == 0) || (length zs == 0) = []
  | otherwise = (head xs, head ys, head zs):(z3 (tail xs) (tail ys) (tail zs))

main = do
  let x = [1, 2, 3]
  let y = [2, 1, 3]
  let z = [1, 2]
  putStrLn $ "X * Y = " ++ (show $ x `dotProduct` y)
  putStrLn . show $ z3 [1, 2, 3] [1, 2] [3, 4, 5, 6]
