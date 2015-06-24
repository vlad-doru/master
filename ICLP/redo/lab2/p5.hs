data Expr a = Term a 
            | Add (Expr a) (Expr a)
            | Subtract (Expr a) (Expr a)
            | Multiply (Expr a) (Expr a)
            | Divide (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  show (Term x) = show x
  show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Subtract x y) = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
  show (Multiply x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Divide x y) = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"

eval :: (Fractional a, Num a) => Expr a -> a
eval (Term x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Subtract x y) = (eval x) - (eval y)
eval (Multiply x y) = (eval x) * (eval y)
eval (Divide x y) = (eval x) / (eval y)

main = do
  let e = Multiply (Add (Term 2) (Term 3)) (Divide (Term 4) (Subtract (Term 4) (Term 2))) :: Expr Double
  putStrLn . show $ e
  putStrLn . show . eval $ e
  
