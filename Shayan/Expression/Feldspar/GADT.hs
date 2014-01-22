{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Expression.Feldspar.GADT where
 
import Variable.GADT
import Data.Array

data Exp r t where 
  ConI :: Integer  -> Exp r Integer 
  ConB :: Bool     -> Exp r Bool
  Var  :: Var r t  -> Exp r t  
  Abs  :: Exp (ta , r) tb -> Exp r (ta -> tb) 
  App  :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r Bool ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (t -> Bool) -> Exp r (t -> t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (tf , ts) 
  Fst  :: Exp r (tf , ts) -> Exp r tf 
  Snd  :: Exp r (tf , ts) -> Exp r ts 
  Ary  :: Exp r Integer -> Exp r (Integer -> t) -> Exp r (Array Integer t)
  Len  :: Exp r (Array Integer t) -> Exp r Integer 
  Ind  :: Exp r (Array Integer t) -> Exp r Integer -> Exp r t 
  Let  :: Exp r tl -> Exp (tl , r) tb -> Exp r tb  
  --  Any  :: Exp r t           
