import Control.Parallel.Strategies
import Control.Concurrent
import Control.Monad

printa = replicateM_ 3 (putStrLn "A" )
printb = replicateM_ 3 (putStrLn "B" )

{-main = do-}
  {-forkIO printa -- se intampla in threadul nou-}
  {-printb -- threadul principal-}

producer :: (MVar String) -> IO ()
producer m = forever $ do
  input <- getLine
  putMVar m input
  return ()

consumer :: (MVar String) -> Integer -> IO()
consumer m n  
  | n == 0 = do 
      putStrLn("Exiting the program.")
      return ()
  | n > 0 = do
      line <- takeMVar m
      putStrLn("Received " ++ line)
      consumer m (n - 1)

main = do
  m <- newEmptyMVar
  forkIO $ producer m -- secondary thread
  consumer m 3 -- main thread
  
  
  
  
