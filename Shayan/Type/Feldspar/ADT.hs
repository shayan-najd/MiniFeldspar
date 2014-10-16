module Type.Feldspar.ADT where

import MyPrelude

data Typ =
    Int
  | Bol
  | Flt
  | Arr Typ Typ
  | Tpl Typ Typ
  | Ary Typ
  | May Typ
  | Cmx

deriving instance Eq   Typ
deriving instance Show Typ