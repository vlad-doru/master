import System.IO

main = interact $ unlines . map (show . rpnEval) . lines

rpnEval :: (Floating a, Fractional a, Read a) => String -> a
rpnEval expr =  head (foldl addTerm [] (words expr))  

addTerm :: (Floating a, Fractional a, Read a) => [a] -> String -> [a]
addTerm stack@(x:rest) term 
  | term == "-" = (y - x):t 
  | term == "+" = (y + x):t
  | term == "*" = (y * x):t
  | term == "/" = (y / x):t
  | term == "ln" = (log x):rest
  | term == "sum" = sum stack:[]
  | otherwise = read term:stack
  where 
    t = tail rest
    y = head rest
addTerm [] term = read term:[]
