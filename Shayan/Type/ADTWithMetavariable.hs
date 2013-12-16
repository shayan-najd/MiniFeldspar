module Type.ADTWithMetavariable where

-- Types with metavariables
data Typ =    
    Mta Int
  | Int   
  | Arr Typ Typ
    deriving Eq
             
instance Show Typ where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Mta n)                    = "a" ++ show n 
             