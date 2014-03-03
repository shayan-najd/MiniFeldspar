module Type.STLC where
 
data Typ =
    Int
  | Arr Typ Typ
  deriving Eq
 
