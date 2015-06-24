fn :: (a -> a) -> Int -> (a -> a)
fn f n 
  | n == 0 = \x -> x
  | n > 0 = f . (fn f (n - 1))
  | otherwise = error "N must be positive."

main = do
  putStrLn . show $ fn (*2) 10 1

