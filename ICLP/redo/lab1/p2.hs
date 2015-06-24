-- Functia reverse pe liste

import System.Environment

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

main = do
  args <- getArgs
  putStrLn $ "Lista inversata de argumente este: " ++ (show . reverseList $ args)
