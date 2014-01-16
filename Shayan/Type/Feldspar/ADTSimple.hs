{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.Feldspar.ADTSimple where

import Control.Monad(unless)

-- Types
data Typ =
    Int
  | Bol
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq

-- Equality between types
(===) :: Monad m => Typ -> Typ -> m ()
t1 === t2 = unless (t1 == t2) (fail "Type Error!" )

instance Show Typ where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (ta@(_  `Arr` _) `Arr` tb) = "(" ++ show ta ++ ") -> " ++ show tb 
  show (ta  `Arr` tb)             = show ta ++ " -> " ++ show tb 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t