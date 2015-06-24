data Persoana = Persoana {nume::String, prenume::String} deriving (Eq)
instance Show Persoana where
  show p = (nume $ p) ++ " " ++ (prenume $ p) 

main = do
  let p = Persoana {nume="Ion", prenume="Vlad-Doru"}
  putStrLn . show $ p
