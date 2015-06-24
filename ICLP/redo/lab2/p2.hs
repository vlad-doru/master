type Point = (Double, Double)
type Radius = Double

class Shape a where
  surface :: a -> Double

data Circle = Circle Point Radius
instance Shape Circle where
  surface (Circle _ r) = pi * r^2

data Rectangle = Rectangle Point Point
instance Shape Rectangle where
  surface (Rectangle (x0, y0) (x1, y1)) = (abs (x0 - x1)) * (abs (y0 - y1))

main = do
  let c = Circle (0, 0) 1
  let r = Rectangle (0, 0) (1, 1)
  putStrLn . show . surface $ c
  putStrLn . show . surface $ r
