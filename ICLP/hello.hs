reverseWords :: String -> String
reverseWords = unwords . reverse . words

main = do   
  line <- getLine  
  if null line  
    then return ()  
  else do  
    putStrLn $ reverseWords line  
    main  
