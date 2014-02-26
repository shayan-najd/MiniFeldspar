{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, GADTs, Rank2Types #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeHoles #-}
module Expression.Feldspar.GADTHigherOrder where
 
import Prelude hiding (sin)
import Variable
import qualified Singleton.TypeFeldspar as G
import Singleton
import qualified Type.Feldspar as A

data Exp :: [A.Typ] -> A.Typ -> * where 
  ConI :: Integer  -> Exp r A.Int 
  ConB :: Bool     -> Exp r A.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: G.Typ ta -> (Exp r ta -> Exp r tb) -> Exp r (A.Arr ta tb) 
  App  :: Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r A.Bol ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (A.Arr t A.Bol) -> Exp r (A.Arr t t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (A.Tpl tf ts) 
  Fst  :: Exp r (A.Tpl tf ts) -> Exp r tf 
  Snd  :: Exp r (A.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r A.Int -> Exp r (A.Arr A.Int t) -> Exp r (A.Ary t)
  Len  :: Exp r (A.Ary t) -> Exp r A.Int 
  Ind  :: Exp r (A.Ary t) -> Exp r A.Int -> Exp r t 
  Let  :: G.Typ tl -> Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb  
  
abs :: HasSin G.Typ ta => (Exp r ta -> Exp r tb) -> Exp r (A.Arr ta tb)
abs = Abs sin 

lett :: HasSin G.Typ tl => Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb  
lett = Let sin

sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll = mapVar Suc (\(Suc x) -> x) 
                           
prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar (\(Suc x) -> x) Suc
               
mapVar :: (forall t'. Var r  t' -> Var r' t') -> 
          (forall t'. Var r' t' -> Var r  t') -> 
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar f _ (Var v)        = Var (f v)
mapVar f g (Abs t eb)     = Abs t (mapVar f g . eb .  mapVar g f)  
mapVar f g (App ef ea)    = App (mapVar f g ef) (mapVar f g ea)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVar f g ec) (mapVar f g eb) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVar f g ef)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let t el eb)  = Let t (mapVar f g el) (mapVar f g . eb . mapVar g f)
 