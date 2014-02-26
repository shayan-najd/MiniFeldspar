{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.Feldspar where

-- Types
data Typ =
    Int
  | Bol
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq 