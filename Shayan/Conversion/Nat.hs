module Conversion.Nat where

import qualified Data.Nat      as A
import qualified Data.Fin      as F
import qualified Singleton.Nat as G
import qualified Variable      as V

import Existential
import Conversion

type ExsNat = ExsSin G.Nat 
type ExsFin = Exs1 F.Nat G.Nat

instance Cnv A.Nat ExsNat where
  cnv A.Zro     = return (ExsSin G.Zro)
  cnv (A.Suc n) = do ExsSin n' <- cnv n
                     return (ExsSin (G.Suc n'))
                     
instance Cnv A.Nat Int where                     
  cnv A.Zro     = return 0
  cnv (A.Suc v) = (1 +) <$> cnv v 
  
instance Cnv (F.Nat n) Int where                     
  cnv F.Zro     = return 0
  cnv (F.Suc v) = (1 +) <$> cnv v 
  
instance Cnv Int A.Nat where                     
  cnv 0         = return A.Zro
  cnv x 
    | x > 0     = A.Suc <$> cnv (pred x)
    | otherwise = fail "Conversion Error!"            
                    
instance Cnv (V.Var e t) A.Nat where                     
  cnv V.Zro     = return A.Zro
  cnv (V.Suc v) = A.Suc <$> cnv v
  
instance Enum A.Nat where
  fromEnum = frmRgt . cnv
  toEnum   = frmRgt . cnv
  
