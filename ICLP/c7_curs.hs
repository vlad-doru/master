


import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM

type Account = TVar Int



deposit :: Account -> Int -> STM ()
deposit acc ammount  = do         
                x <- readTVar acc
                writeTVar acc (x + ammount)
                
                                         
withdraw  :: Account -> Int -> STM ()
withdraw  acc ammount  = do         
                x <- readTVar acc                
                writeTVar acc (x - ammount)
    

  
transfer :: Account -> Account -> Int -> IO()
transfer from to ammount = atomically  $ do 
                                        withdraw from ammount 
                                        deposit to ammount
                                         
                   
                   
                   
                 
extras :: Account -> String -> IO()
extras acc str =  do
                            x <- atomically $ readTVar acc 
                            putStrLn ("Contul " ++ str ++ ": " ++ (show x))
                            


                            

 

      
      
      
main = do 
       --aMVar <- atomically $ newTVar 1000       
       --bMVar <- atomically $ newTVar 1000       
       (a,b) <- atomically $ do
                     a <- newTVar 1000
                     b <- newTVar 1000
                     return (a,b)
       forkIO(transfer a b 300)
       forkIO (transfer b a 500)
       threadDelay $ 2 * (10^6)
       extras a "a"
       extras b "b"       
       
   
      
     
 
   

    
   
