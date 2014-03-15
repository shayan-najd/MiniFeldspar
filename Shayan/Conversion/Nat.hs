module Conversion.Nat () where

import Prelude ()
import MyPrelude 

import qualified Nat.ADT  as NA
import qualified Nat.GADT as NG

import Conversion
 
instance Cnv NA.Nat (ExsSin NG.Nat) where
  cnv NA.Zro     = return (ExsSin NG.Zro)
  cnv (NA.Suc n) = do ExsSin n' <- cnv n
                      return (ExsSin (NG.Suc n'))
                     
instance Cnv NA.Nat Int where                     
  cnv NA.Zro     = return 0
  cnv (NA.Suc v) = (1 +) <$> cnv v 
  
instance Cnv Int NA.Nat where                     
  cnv 0         = return NA.Zro
  cnv x 
    | x > 0     = NA.Suc <$> cnv (pred x)
    | otherwise = fail "Conversion Error!"            
                      
instance Enum NA.Nat where
  fromEnum = frmRgt . cnv
  toEnum   = frmRgt . cnv
  
