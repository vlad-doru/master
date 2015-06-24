-- Definim in alt mod show pt MyTree

data MyTree a = NilTree | Node a (MyTree a) (MyTree a)
instance (Show a) => Show (MyTree a) where
  show NilTree = "[]"
  show (Node x left right) = "[" ++ (show x) ++ (show left) ++ (show right) ++ "]"

main = do
  let t = Node 1 (Node 2 NilTree NilTree) NilTree
  putStrLn . show $ t
