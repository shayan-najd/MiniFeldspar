module Variable.ADT where

-- Variables are represented as natural numbers
data Var =
    Zro
  | Suc Var
  deriving Eq

type Nat = Var

instance Show Var where
 show = ("x"++) . show . int

int :: Var -> Int  
int Zro     =  0
int (Suc v) =  1 + int v 
