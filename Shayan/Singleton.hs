module Singleton where
  
import ErrorMonad

class HasSin tf t where
  sin :: tf t
 
type family Trm (t :: k) :: *
     
type family RevTrm (t :: *) :: k     
     
data Eql a b where
  Rfl :: Eql a a 
 
class EqlSin tf where
  eqlSin :: tf t -> tf t' -> ErrM (Eql t t')
