{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts , NoMonomorphismRestriction #-}
module Expression.STLC.GADTHigherOrder(Exp(..),abs,sucAll,prdAll) where
 
import Prelude hiding (sin,abs)
import Variable.GADT
import Type.STLC.GADT
import Singleton
import Singleton.TypeSTLC as S () 

-- Higher-Order GADT representation (Debruijn indices) of the simply-typed 
-- lambda calculus expressions with Integer constants and a built-in addition 
-- operator
data Exp r t where
  Var :: Var r t -> Exp r t
  Con :: Integer -> Exp r Integer
  Add :: Exp r Integer -> Exp r Integer -> Exp r Integer
  Abs :: Typ ta -> (Exp r ta -> Exp r tb) -> Exp r (ta -> tb)
  App :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb 
                        
abs :: Sin Typ ta => (Exp r ta -> Exp r tb) -> Exp r (ta -> tb)
abs = Abs sin 

sucAll :: Exp r t' -> Exp (t , r) t' 
sucAll (Con i)     = Con i
sucAll (Var v)     = Var (Suc v)
sucAll (Add el er) = Add (sucAll el) (sucAll er)
sucAll (App ef ea) = App (sucAll ef) (sucAll ea)
sucAll (Abs t f)   = Abs t (sucAll . f . prdAll)  

-- Should not contain variable zro
prdAll :: Exp (t , r) t' -> Exp r t'
prdAll (Con i)       = Con i
prdAll (Var (Suc v)) = Var v
prdAll (Var Zro)     = error "Impossible!"
prdAll (Add el er)   = Add (prdAll el) (prdAll er)
prdAll (App ef ea)   = App (prdAll ef) (prdAll ea)
prdAll (Abs t f)     = Abs t (prdAll . f . sucAll) 
  