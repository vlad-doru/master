import Control.Monad
import Control.Concurrent
import Control.Concurrent.QSem

seconds = 10^6

master :: (MVar String) -> Integer -> IO ()
master m iterations
  | iterations == 0 = do
      putStrLn "Exit the program."
      return ()
  | otherwise = do
      message <- takeMVar m
      putStrLn $ message
      master m (iterations - 1)
      return ()

worker :: Integer -> (MVar String) -> QSem -> IO ()
worker index m s = forever $ do
  waitQSem s -- aquire the semaphore
  putMVar  m ("Worker " ++ (show index) ++ " aquired the semaphore.")
  {-threadDelay $ 2 * seconds-}
  signalQSem s -- release the semaphore
  putMVar  m ("Worker " ++ (show index) ++ " released the semaphore.")
  {-threadDelay $ 2 * seconds-}

main = do
  m <- newEmptyMVar
  s <- newQSem 3
  mapM_ (\index -> forkIO $ worker index m s) [1..5]
  master m 10
