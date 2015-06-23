import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Read
import Text.Printf

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop where
  loop = do
    l <- hGetLine h
    if 
      l == "end"
    then 
      do
        hPutStrLn h "Closing the connection with the server. Thanks for using us!"
        hClose h
    else
      do
        let nr = readMaybe l :: (Maybe Integer)
        case nr of
          Nothing -> hPrintf h "This line cannor be converted to an int: %s\n" l
          Just x -> hPutStrLn h (show (2 * x))
        loop

port = 8888 :: Integer

main = do 
  sock <- listenOn(PortNumber(fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s:%s\n" host (show port)
    forkIO $ talk handle


  
