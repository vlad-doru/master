-- Rezolvarea functiei de gradul 2.
import Text.Read
import Text.Printf

type Solution = Either Double (Double, Double)

solve2 :: Double -> Double -> Double -> Solution
solve2 a b c 
  | delta < 0 = error "Negative delta"
  | delta == 0 = Left ((-b) / (2 * a))
  | otherwise = Right (((-b) + (sqrt delta)) / (2 * a), ((-b) - (sqrt delta)) / (2 * a))
  where delta = b^2 - 4*a*c

getNumber :: (Read a) => String -> IO a
getNumber message = do
  print message
  x <- getLine
  let n = read x 
  return n

main = do
  a <- getNumber "a = "
  b <- getNumber "b = "
  c <- getNumber "c = "
  let sol = solve2 a b c 
  case sol of
    Left s -> printf "Solutie unica %f\n" s
    Right sols -> putStrLn $ "Solutiile sunt: " ++ (show sols)
