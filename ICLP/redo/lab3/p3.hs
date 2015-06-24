-- Cautare de cuvant pe linii
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Environment

isInLine :: String -> String -> IO Int
isInLine l w = do
  if elem w (words l)
  then return 1
  else return 0

main = do
  [file, word] <- getArgs
  contents <- readFile file
  let ls = lines contents
  as <- mapM (\l -> async $ isInLine l word) ls
  results <- mapM wait as
  let occurences = sum results  
  putStrLn $ "Cuvantul apare de: " ++ (show occurences)
   
