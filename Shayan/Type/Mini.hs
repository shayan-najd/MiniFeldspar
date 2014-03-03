module Type.Mini where

data Typ =
    Int
  | Bol
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq
 
