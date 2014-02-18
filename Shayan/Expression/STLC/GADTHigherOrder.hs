{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts , NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Expression.STLC.GADTHigherOrder(Exp(..),abs,sucAll,prdAll) where
 
import Prelude hiding (sin,abs)
import Variable.GADT
import Singleton.TypeSTLC
import Singleton
import Singleton.TypeSTLC as S () 
import qualified Type.STLC.ADTSimple as A

data Exp :: [A.Typ] -> A.Typ -> * where
  Var :: Var r t -> Exp r t
  Con :: Integer -> Exp r A.Int
  Add :: Exp r A.Int -> Exp r A.Int -> Exp r A.Int
  Abs :: Typ ta -> (Exp r ta -> Exp r tb) -> Exp r (A.Arr ta tb)
  App :: Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb 
                        
abs :: HasSin Typ ta => (Exp r ta -> Exp r tb) -> Exp r (A.Arr ta tb)
abs = Abs sin 

sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll (Con i)     = Con i
sucAll (Var v)     = Var (Suc v)
sucAll (Add el er) = Add (sucAll el) (sucAll er)
sucAll (App ef ea) = App (sucAll ef) (sucAll ea)
sucAll (Abs t f)   = Abs t (sucAll . f . prdAll)  

-- Should not contain variable zro
prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll (Con i)       = Con i
prdAll (Var (Suc v)) = Var v
prdAll (Var Zro)     = error "Impossible!"
prdAll (Add el er)   = Add (prdAll el) (prdAll er)
prdAll (App ef ea)   = App (prdAll ef) (prdAll ea)
prdAll (Abs t f)     = Abs t (prdAll . f . sucAll) 
  