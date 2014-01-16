{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.Feldspar.ADTWithMetavariable where

import Variable.ADT

-- Types with metavariables
data Typ =    
    Mta Nat
  | Int   
  | Bol
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  
    deriving Eq
             
instance Show Typ where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Mta n)                    = "a" ++ show n 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t
             