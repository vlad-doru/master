import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random
import Text.Printf

type Fork = TVar Bool -- true means that it is free
data Philosopher = Philosopher {name::String, 
                                left::Fork,
                                right::Fork}

seconds :: Int
seconds = 10 ^ 6

newFork :: IO (Fork)
newFork = atomically $ do
  newTVar True

takeFork :: Fork -> STM ()
takeFork f = do
  free <- readTVar f
  if free 
  then writeTVar f False
  else retry

dropFork :: Fork -> STM ()
dropFork f = do writeTVar f True

-- Used for eating and thinking.
waitRandom :: IO ()
waitRandom = do
  delay <- randomRIO(1, 5)
  threadDelay $ delay * seconds

runPhilosopher :: Philosopher -> IO()
runPhilosopher p = forever $ do
  printf "%s is hungry.\n" (name p)
  -- Takes the forks
  atomically $ do
    takeFork $ left p
    takeFork $ right p
  printf "%s starts to eat.\n" (name p)
  -- Eats for a random period of time
  waitRandom
  -- Drops the fork
  atomically $ do
    dropFork $ left p
    dropFork $ right p
  printf "%s is thinking.\n" (name p)
  waitRandom

names  = ["Aristotle", "Kant", "Spinoza",  "Marx", "Russel"] 

setTable :: [String] -> IO ([Fork], [Philosopher])
setTable names = do
  forks <- mapM (\_ -> newFork) names
  let n = length forks
  let philosophers = map (\(i, name) -> Philosopher {name = name, 
                                                     left = (forks!!(i `mod` n)),
                                                     right = (forks!!((i + 1) `mod` n))}) (zip [1..n] names)
  return (forks, philosophers)

main = do
  (forks, philosophers) <- setTable names
  mapM_ (forkIO . runPhilosopher) philosophers
  threadDelay $ 50 * seconds
