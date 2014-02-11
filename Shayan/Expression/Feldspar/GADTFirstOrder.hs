{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs , FlexibleContexts #-}
module Expression.Feldspar.GADTFirstOrder where
 
import Prelude hiding (sin)
import Variable.GADT
import Data.Array
import Type.Feldspar.GADT
import Singleton

data Exp r t where 
  ConI :: Integer  -> Exp r Integer 
  ConB :: Bool     -> Exp r Bool
  Var  :: Var r t  -> Exp r t  
  Abs  :: Typ ta -> Exp (ta , r) tb -> Exp r (ta -> tb) 
  App  :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r Bool ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (t -> Bool) -> Exp r (t -> t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (tf , ts) 
  Fst  :: Exp r (tf , ts) -> Exp r tf 
  Snd  :: Exp r (tf , ts) -> Exp r ts 
  Ary  :: Exp r Integer -> Exp r (Integer -> t) -> Exp r (Array Integer t)
  Len  :: Exp r (Array Integer t) -> Exp r Integer 
  Ind  :: Exp r (Array Integer t) -> Exp r Integer -> Exp r t 
  Let  :: Typ tl -> Exp r tl -> Exp (tl , r) tb -> Exp r tb  
  --  Any  :: Exp r t           

abs :: Sin Typ ta => Exp (ta , r) tb -> Exp r (ta -> tb) 
abs = Abs sin  

lett :: Sin Typ tl => Exp r tl -> Exp (tl , r) tb -> Exp r tb  
lett = Let sin