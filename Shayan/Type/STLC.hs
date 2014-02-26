{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.STLC where
 
-- Types
data Typ =
    Int
  | Arr Typ Typ
  deriving Eq
 
