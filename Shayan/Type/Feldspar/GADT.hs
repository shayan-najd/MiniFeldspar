{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Type.Feldspar.GADT where

import Data.Array

-- Types (Singleton)
data Typ t where
  Int :: Typ Integer
  Bol :: Typ Bool 
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  Tpl :: Typ tf -> Typ ts -> Typ (tf , ts)
  Ary :: Typ t  -> Typ (Array Integer t)
--  Any :: Typ t
  
instance Show (Typ t) where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t
--  show (Any)                      = "∀a.a" 