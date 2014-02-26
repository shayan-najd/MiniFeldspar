{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.Mini where

-- Types
data Typ =
    Int
  | Bol
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq
 
