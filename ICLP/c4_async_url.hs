import Control.Monad
import Control.Concurrent
import System.Environment
import qualified Data.ByteString as B
import GetURL
import TimeIt
import Text.Printf
import Control.Concurrent.Async

action url = do
  (page, time) <- timeit $ getURL url
  printf "%s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
  [file] <- getArgs
  urls <- fmap lines (readFile file)
  actions <- mapM (async . action) urls
  mapM_ wait actions

