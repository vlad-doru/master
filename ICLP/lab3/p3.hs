import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Environment

isInLine :: String -> String -> IO Int
isInLine l w = do
  return $ if elem w (words l) then 1 else 0 

linesWithWord :: [String] -> String -> IO Int
linesWithWord ls w = do
  as <- mapM (\l -> async $ isInLine l w ) ls
  results <- mapM wait as
  return $ sum results

main = do
  [file, word] <- getArgs 
  contents <- readFile(file) 
  let ls = lines contents
  l <- linesWithWord ls word
  putStrLn $ "The number of lines with the given word is " ++ (show l)
