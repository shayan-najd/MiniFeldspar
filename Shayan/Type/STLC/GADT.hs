{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Type.STLC.GADT where

-- Types (Singleton)
data Typ t where
  Int :: Typ Integer
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  
instance Show (Typ t) where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 


