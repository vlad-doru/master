import qualified Data.List

-- PRBOLEMA 1 --
-- Definiti  in alt mod show pentru tipul de date  MyTree a definit la curs, folosind exemplul din curs. 

data MyTree a = NilTree | Node a (MyTree a) (MyTree a)

instance (Show a) => Show (MyTree a) where
  show NilTree = "[]"
  show (Node x l r) = "[" ++ (show x) ++ " L:" ++ (show l) ++ " R:" ++ (show r) ++ "]"

-- PROBLEMA 2 --
-- Definiti un tip de date ale carui valori sa fie figuri geometrice din plan(dreptunghiuri, cercuri); pentru datele de acest tip definiti aria.  

-- Create our own typeclass.
class Shape a where
  surface :: a -> Double 

type Point = (Double, Double)

data Circle = Circle Point Double -- Origin and radius
instance Shape Circle where
  surface (Circle _ r) = pi * (r^2)

data Rectangle = Rectangle Point Point -- Bottom-left and top-right points
instance Shape Rectangle where
  surface (Rectangle (x0, y0) (x1, y1)) = (abs (x1 - x0)) * (abs(y1 - y0))

-- PROBLEMA 3 --
-- Definti un tip de date record Persoana asemanator celui din curs; faceti acest tip instanta a claselor Eq si Show (verificati deriving si construiti o instantiere proprie). 

data Persoana = Persoana {nume::String, prenume::String} deriving (Eq)
instance Show Persoana where
  show p = "Nume: " ++ (nume $ p) ++ " Prenume: " ++ (prenume $ p)

-- PROBLEMA 4 --
-- Definiti tipul de date (parametrizat) multime direct sau plecand de la liste; faceti acest tip instanta a claselor Eq si Show; definiti fucntii pentru operatii specifice de multimi; definiti o functie pentru multimea partilor.


data Multime a = Vida | Elem a (Multime a) deriving (Show, Ord, Eq) 

isElem :: (Eq a) => a -> Multime a -> Bool
isElem x Vida = False
isElem x (Elem e m)
  | x == e = True
  | otherwise = isElem x m

insert :: (Eq a, Ord a) => a -> Multime a -> Multime a
insert x Vida = Elem x Vida
insert x m@(Elem y rest) 
  | x <= y = Elem x m 
  | otherwise = Elem y (insert x rest)

fromList :: (Eq a, Ord a) => [a] -> Multime a
fromList x = foldl (\m x -> insert x m) Vida x

union :: (Eq a, Ord a) => Multime a -> Multime a -> Multime a
union Vida x = x
union x Vida = x
union m1@(Elem x r1) m2@(Elem y r2) 
  | x == y = Elem x (union r1 r2)
  | x < y = Elem x (union r1 m2)
  | x > y = Elem y (union m1 r2)

intersect :: (Eq a, Ord a) => Multime a -> Multime a -> Multime a
intersect Vida _ = Vida
intersect _ Vida = Vida
intersect m1@(Elem x r1) m2@(Elem y r2) 
  | x == y = Elem x (intersect r1 r2)
  | x < y = intersect r1 m2
  | x > y = intersect m1 r2

parti :: (Eq a, Ord a) => Multime a -> [Multime a]
parti Vida = [Vida]
parti (Elem x m) = Data.List.sort ((parti m) ++ (map (\m -> insert x m) (parti m)))

-- PROBLEMA 5 --
-- Definti un tip de date propriu pentru expresii aritmetice (operatorii pot fi cei uzuali dar si altii  noi); faceti acest tip instanta a clasei Show; definiti o functie de evaluare a acestor expresii.   

data Expr = Nr Double | Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr

instance Show Expr where
  show (Nr x) = show x
  show (Add e1 e2) = "(" ++ (show e1) ++ ")+(" ++ (show e2) ++ ")"
  show (Subtract e1 e2) = "(" ++ (show e1) ++ ")-(" ++ (show e2) ++ ")"
  show (Multiply e1 e2) = (show e1) ++ "*" ++ (show e2) 
  show (Divide e1 e2) = (show e1) ++ "/" ++ (show e2) 

eval :: Expr -> Double
eval (Nr x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Subtract e1 e2) = (eval e1) - (eval e2)
eval (Multiply e1 e2) = (eval e1) * (eval e2)
eval (Divide e1 e2) = (eval e1) / (eval e2)

-- PROBLEMA 6 -- 
-- Definiti un tip de date myJSON asemanator celui original  (se defineste recursiv: se definesc tipurile de baza, iar tipurile compuse sunt obiecte si liste; obiectele sunt liste  de perechi (String, valoare) ). faceti acest tip instanta a clasei Show.  

-- TODO!
