module Data.Nat where

-- Natural numbers
data Nat =
    Zro
  | Suc Nat
  deriving Eq

instance Enum Nat where
  fromEnum = cnvVarInt
  toEnum   = cnvIntVar

instance Show Nat where
 show = show . cnvVarInt

cnvVarInt :: Nat -> Int  
cnvVarInt Zro     = 0
cnvVarInt (Suc v) = 1 + cnvVarInt v 

cnvIntVar :: Int -> Nat
cnvIntVar 0   = Zro
cnvIntVar x 
  | x > 0     = Suc (cnvIntVar (pred x))
  | otherwise = error "Conversion Error!"            


