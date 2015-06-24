import Text.Printf

data MyJSON = JSString String 
            | JSFloat Float
            | JSDouble Double
            | JSInt Int
            | JSBool Bool
            | JSList [MyJSON]
            | JSObject [(String, MyJSON)] deriving (Eq)

instance Show MyJSON where
  show (JSString s) = s
  show (JSFloat x) = show x
  show (JSDouble x) = show x
  show (JSInt x) = show x
  show (JSBool b) = show b
  show (JSList l) = show l
  show (JSObject o) = "{" ++ (printList o) ++ "}"

printList :: [(String, MyJSON)] -> String
printList [] = ""
printList ((s, l):o) = s ++ ": " ++ (show l) ++ " \n " ++ (printList o)

main = do
  let o = JSObject [("index1", (JSList [JSBool True, JSDouble 3.14])), 
                    ("index2", (JSList [JSString "placinta", JSInt 10]))] 
  print . show $ o
