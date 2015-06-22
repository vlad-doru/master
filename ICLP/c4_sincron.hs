-- Comunicare sincrona --
-- Un thread – reader- citeste un fisier text linie cu linie
--
-- Liniile sunt trimise, pe rand, 
-- unui al doilea thread – writer- care le afiseaza 
-- in ordinea trimisa si le numara.
--
-- La sfarsit,  thread-ul writer trimite thread-ului reader 
-- numarul de linii si acesta il afiseaza,

import Control.Monad
import Control.Concurrent
import System.Environment
import System.IO

type Message = Maybe String

master :: String -> (Chan Message) -> (MVar Integer) -> IO ()
master file c m = do
  text <- readFile file
  let contents = lines text
  putMessage contents
  n <- takeMVar m
  putStrLn $ "Count: " ++ (show n)
  return ()
  where 
    putMessage contents 
      | contents == [] = do
          writeChan c Nothing
          print "Finished streaming."
      | otherwise = do
          writeChan c (Just (head contents))
          putMessage $ tail contents

worker :: (Chan Message) -> (MVar Integer) -> IO ()
worker c m = loop 0 where
  loop i = do
    s <- readChan c
    case s of
      Nothing -> putMVar m i 
      (Just _) -> loop (i + 1) 
  

main = do
  [file] <- getArgs
  c <- newChan
  m <- newEmptyMVar
  forkIO $ worker c m
  master file c m
