import qualified Data.List

data Multime a = Vida | Elem a (Multime a) deriving (Ord, Eq) 

instance (Show a) => Show (Multime a) where
  show m = show . toList $ m

isElem :: (Eq a) => a -> Multime a -> Bool
isElem x Vida = False
isElem x (Elem e m)
  | x == e = True
  | otherwise = isElem x m

insert :: (Eq a, Ord a) => a -> Multime a -> Multime a
insert x Vida = Elem x Vida
insert x m@(Elem y rest) 
  | x < y = Elem x m 
  | x == y = m
  | otherwise = Elem y (insert x rest)

fromList :: (Eq a, Ord a) => [a] -> Multime a
fromList x = foldl (\m x -> insert x m) Vida x

toList :: Multime a -> [a]
toList Vida = []
toList (Elem x m) = x:(toList m)

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

main = do 
  let s = fromList [2, 3, 1, 2, 3, 1, 1, 1, 5, 4]
  let t = fromList [0, -1, 0, -1]
  let z = union s t
  putStrLn . show $ z
