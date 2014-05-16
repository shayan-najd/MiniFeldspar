module Type.Feldspar.ADT where

import Prelude ()
import MyPrelude

data Typ =
    Int
  | Bol
  | Flt  
  | Arr Typ Typ
  | Tpl Typ Typ  
  | Ary Typ  
  | Cmx  

deriving instance Eq   Typ 
deriving instance Show Typ