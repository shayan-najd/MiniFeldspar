{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs , FlexibleContexts, Rank2Types  #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeHoles #-}
module Expression.Feldspar.GADTFirstOrder where
 
import Prelude hiding (sin)
import Variable.GADT
import qualified Singleton.TypeFeldspar as G
import Singleton
import qualified Type.Feldspar as A

data Exp :: [A.Typ] -> A.Typ -> * where 
  ConI :: Integer  -> Exp r A.Int 
  ConB :: Bool     -> Exp r A.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: G.Typ ta -> Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
  App  :: Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r A.Bol ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (A.Arr t A.Bol) -> Exp r (A.Arr t t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (A.Tpl tf ts) 
  Fst  :: Exp r (A.Tpl tf ts) -> Exp r tf 
  Snd  :: Exp r (A.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r A.Int -> Exp r (A.Arr A.Int t) -> Exp r (A.Ary t)
  Len  :: Exp r (A.Ary t) -> Exp r A.Int 
  Ind  :: Exp r (A.Ary t) -> Exp r A.Int -> Exp r t 
  Let  :: G.Typ tl -> Exp r tl -> Exp (tl ': r) tb -> Exp r tb  

abs :: HasSin G.Typ ta => Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
abs = Abs sin  

lett :: HasSin G.Typ tl => Exp r tl -> Exp (tl ': r) tb -> Exp r tb  
lett = Let sin
 
sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t' 
prdAll = mapVar (\(Suc x) -> x)
               
inc :: (forall t'. Var r t' -> Var r' t') -> Var (ta ': r) t -> Var (ta ': r') t
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)
 
mapVar :: (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar _ (ConI i)       = ConI i
mapVar _ (ConB i)       = ConB i
mapVar f (Var v)        = Var (f v)
mapVar f (Abs t  eb)    = Abs t (mapVar (inc f) eb)  
mapVar f (App ef ea)    = App (mapVar f ef) (mapVar f ea)
mapVar f (Cnd ec et ef) = Cnd (mapVar f ec) (mapVar f et) (mapVar f ef)
mapVar f (Whl ec eb ei) = Whl (mapVar f ec) (mapVar f eb) (mapVar f ei)
mapVar f (Tpl ef es)    = Tpl (mapVar f ef) (mapVar f es)
mapVar f (Fst e)        = Fst (mapVar f e)
mapVar f (Snd e)        = Snd (mapVar f e)
mapVar f (Ary el ef)    = Ary (mapVar f el) (mapVar f ef)
mapVar f (Len e)        = Len (mapVar f e)
mapVar f (Ind ea ei)    = Ind (mapVar f ea) (mapVar f ei)
mapVar f (Let t el eb)  = Let t (mapVar f el) (mapVar (inc f) eb)

 