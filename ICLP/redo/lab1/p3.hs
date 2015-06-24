-- Verifica daca un sir e palindrom

import System.Environment

isPali :: String -> Bool
isPali x
  | x == "" = True
  | (length x) == 1 = True
  | (head x) == (last x) = isPali . tail . init $ x
  | otherwise = False

main = do 
  s <- getLine
  putStrLn $ "Este palindom? " ++ (show . isPali $ s)
