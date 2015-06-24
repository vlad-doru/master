map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f x y 
  | length x == length y = map (uncurry f) (zip x y)
  | otherwise = error "Lungimi diferite ale lsitelor."

main = do
  putStrLn . show $ map2 (+) [1, 2, 3] [3, 0, -1]
  putStrLn . show $ map2 (+) [1, 2, 3] [3, 0]
