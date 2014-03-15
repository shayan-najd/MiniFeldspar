module Singleton where
  
import Prelude ()
import MyPrelude  

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

samTyp :: tf t -> ef t -> ef t
samTyp _ = id 

data T t = T 