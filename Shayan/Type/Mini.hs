{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.Mini where

import Control.Monad(unless)

-- Types
data Typ =
    Int
  | Bol
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq

-- Equality between types
(===) :: Monad m => Typ -> Typ -> m ()
t1 === t2 = unless (t1 == t2) (fail "Type Error!" )

instance Show Typ where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t