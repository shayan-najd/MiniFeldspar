module Type.Feldspar.ADT where

import Prelude ()
import MyPrelude

data Typ =
    Int
  | Bol
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  

deriving instance Eq   Typ 
deriving instance Show Typ