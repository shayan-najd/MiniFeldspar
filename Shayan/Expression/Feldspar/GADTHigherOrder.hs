{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Expression.Feldspar.GADTHigherOrder where
 
import Prelude hiding (sin)
import Variable.GADT
import qualified Singleton.TypeFeldspar as G
import Singleton
import qualified Type.Feldspar as A

data Exp :: [A.Typ] -> A.Typ -> * where 
  ConI :: Integer  -> Exp r A.Int 
  ConB :: Bool     -> Exp r A.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: G.Typ ta -> (Exp r ta -> Exp r tb) -> Exp r (ta `A.Arr` tb) 
  App  :: Exp r (ta `A.Arr` tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r A.Bol ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (t `A.Arr` A.Bol) -> Exp r (t `A.Arr` t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (A.Tpl tf ts) 
  Fst  :: Exp r (A.Tpl tf ts) -> Exp r tf 
  Snd  :: Exp r (A.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r A.Int -> Exp r (A.Int `A.Arr` t) -> Exp r (A.Ary t)
  Len  :: Exp r (A.Ary t) -> Exp r A.Int 
  Ind  :: Exp r (A.Ary t) -> Exp r A.Int -> Exp r t 
  Let  :: G.Typ tl -> Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb  
  
abs :: HasSin G.Typ ta => (Exp r ta -> Exp r tb) -> Exp r (ta `A.Arr` tb)
abs = Abs sin 

sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll (ConI i)       = ConI i
sucAll (ConB i)       = ConB i
sucAll (Var v)        = Var (Suc v)
sucAll (Abs t f)      = Abs t (sucAll . f . prdAll)  
sucAll (App ef ea)    = App (sucAll ef) (sucAll ea)
sucAll (Cnd ec et ef) = Cnd (sucAll ec) (sucAll et) (sucAll ef)
sucAll (Whl ec eb ei) = Whl (sucAll ec) (sucAll eb) (sucAll ei)
sucAll (Tpl ef es)    = Tpl (sucAll ef) (sucAll es)
sucAll (Fst e)        = Fst (sucAll e)
sucAll (Snd e)        = Snd (sucAll e)
sucAll (Ary el ef)    = Ary (sucAll el) (sucAll ef)
sucAll (Len e)        = Len (sucAll e)
sucAll (Ind ea ei)    = Ind (sucAll ea) (sucAll ei)
sucAll (Let t el eb)  = Let t (sucAll el) (sucAll . eb . prdAll)

-- Should not contain variable zro
prdAll :: Exp (t ': r) t' -> Exp r t' 
prdAll (ConI i)       = ConI i
prdAll (ConB i)       = ConB i
prdAll (Var (Suc v))  = Var v
prdAll (Var Zro)      = error "Impossible!"
prdAll (Abs t f)      = Abs t (prdAll . f . sucAll)  
prdAll (App ef ea)    = App (prdAll ef) (prdAll ea)
prdAll (Cnd ec et ef) = Cnd (prdAll ec) (prdAll et) (prdAll ef)
prdAll (Whl ec eb ei) = Whl (prdAll ec) (prdAll eb) (prdAll ei)
prdAll (Tpl ef es)    = Tpl (prdAll ef) (prdAll es)
prdAll (Fst e)        = Fst (prdAll e)
prdAll (Snd e)        = Snd (prdAll e)
prdAll (Ary el ef)    = Ary (prdAll el) (prdAll ef)
prdAll (Len e)        = Len (prdAll e)
prdAll (Ind ea ei)    = Ind (prdAll ea) (prdAll ei)
prdAll (Let t el eb)  = Let t (prdAll el) (prdAll . eb . sucAll)