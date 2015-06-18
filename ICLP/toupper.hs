import Data.Char

-- Put only lines shorter than 10 chars
{-main = do-}
  {-contents <- getContents-}
  {-putStr (shortLines contents)-}

{-shortLines :: String -> String-}
{-shortLines input = -}
  {-let allLines = lines input-}
      {-shortLines = filter (\ line -> length line < 10) allLines-}
  {-in unlines shortLines-}

-- Shorter way to do it
{-main = interact $ unlines . filter (\ l -> length l < 10) . lines-}

-- Continously read and tell us if palindrome
main = interact $ unlines . map (\l -> if (l == reverse l) then "YES" else "NO") . lines
