module Singleton where
  
import MyPrelude  

import qualified Nat.ADT as NA

class HasSin tf t where
  sin :: tf t
 
type family Trm (t :: k) :: *
     
type family RevTrm (t :: *) :: k     
    
data Eql :: k -> k -> * where
  Rfl :: Eql a a 
 
class EqlSin tf where
  eqlSin :: tf t -> tf t' -> ErrM (Eql t t')

data PrfHasSin :: (k -> *) -> k -> *  where
  PrfHasSin :: HasSin tf t => PrfHasSin tf t
  
class GetPrfHasSin tf where
  getPrfHasSin :: tf t -> PrfHasSin tf t
  
getPrfHasSinM :: (GetPrfHasSin tf, Monad m) =>
                 tf t -> m (PrfHasSin tf t)
getPrfHasSinM = return . getPrfHasSin

sinTyp :: HasSin tf t => ef t -> tf t
sinTyp _ = sin

sinTypOf :: HasSin tf t => ef t -> tf t' -> tf t
sinTypOf _ _ = sin

samTyp :: tf t -> ef t -> ef t
samTyp _ = id 

samTypM :: tf t -> m(ef t) -> m(ef t)
samTypM _ = id 

data T t = T 

type family Len (l :: [k]) :: NA.Nat where
  Len (x ': xs) = NA.Suc (Len xs)
  Len '[]       = NA.Zro 

type family Add (ll :: [k]) (lr :: [k]) :: [k]  where
  Add '[]       lr = lr
  Add (x ': xs) lr = x ': Add xs lr