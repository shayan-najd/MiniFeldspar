module Type.Feldspar where

data Typ =
    Int
  | Bol
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  
  deriving Eq 