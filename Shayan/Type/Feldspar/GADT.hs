module Type.Feldspar.GADT where

import Prelude ()
import MyPrelude

import qualified Type.Feldspar.ADT as A

import Singleton
 
data Typ :: A.Typ -> * where
  Int :: Typ A.Int 
  Bol :: Typ A.Bol 
  Arr :: Typ ta -> Typ tb -> Typ (A.Arr ta tb)
  Tpl :: Typ tf -> Typ ts -> Typ (A.Tpl tf ts)
  Ary :: Typ t  -> Typ (A.Ary t)

type instance Trm A.Int         = Integer
type instance Trm A.Bol         = Bool
type instance Trm (A.Arr ta tb) = Trm ta -> Trm tb
type instance Trm (A.Ary ta)    = Array Integer (Trm ta)
type instance Trm (A.Tpl tf ts) = (Trm tf , Trm ts)
  
type instance RevTrm Integer            = A.Int 
type instance RevTrm Bool               = A.Bol
type instance RevTrm (ta -> tb)         = A.Arr (RevTrm ta) (RevTrm tb) 
type instance RevTrm (tf , ts)          = A.Tpl (RevTrm tf) (RevTrm ts) 
type instance RevTrm (Array Integer ta) = A.Ary (RevTrm ta)

instance HasSin Typ A.Int where
  sin = Int 
 
instance HasSin Typ A.Bol where
  sin = Bol 

instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (A.Arr ta tb) where
  sin = Arr sin sin
  
instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (A.Tpl tf ts) where
  sin = Tpl sin sin  
  
instance HasSin Typ ta => HasSin Typ (A.Ary ta) where
  sin = Ary sin  
  
instance EqlSin Typ where
  eqlSin Int         Int           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (Ary t)     (Ary t')      = do Rfl <- eqlSin t t'
                                        return Rfl  
  eqlSin _              _          = fail "Type Error!"
  
instance GetPrfHasSin Typ where
  getPrfHasSin t  = case t of  
    Int       -> PrfHasSin
    Bol       -> PrfHasSin
    Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Ary ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin

getPrfHasSinArr :: forall ta tb t. HasSin Typ (A.Arr ta tb) => 
                   t (A.Arr ta tb) -> (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArr _ = case sin :: Typ (A.Arr ta tb) of 
  Arr ta tb -> (getPrfHasSin ta , getPrfHasSin tb)
 
getPrfHasSinTpl :: forall tf ts t. HasSin Typ (A.Tpl tf ts) => 
                   t (A.Tpl tf ts) -> (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTpl _ = case sin :: Typ (A.Tpl tf ts) of 
  Tpl tf ts -> (getPrfHasSin tf , getPrfHasSin ts)

getPrfHasSinAry :: forall ta t. HasSin Typ (A.Ary ta) => 
                   t (A.Ary ta) -> PrfHasSin Typ ta
getPrfHasSinAry _ = case sin :: Typ (A.Ary ta) of 
  Ary ta    -> getPrfHasSin ta

getPrfHasSinArrM :: HasSin Typ (A.Arr ta tb) => 
                    t (A.Arr ta tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: HasSin Typ (A.Tpl tf ts) => 
                    t (A.Tpl tf ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

getPrfHasSinAryM :: HasSin Typ (A.Ary ta) => 
                   t (A.Ary ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinAryM = return  . getPrfHasSinAry

type family Out (t :: A.Typ) :: A.Typ where 
  Out (A.Arr ta tb) = Out tb
  Out t             = t
    
type family Arg (t :: A.Typ) :: [A.Typ] where
  Arg (A.Arr ta tb) = ta ': Arg tb
  Arg t             = '[] 
  
getArgTyp :: Typ (A.Arr ta tb) -> Typ ta  
getArgTyp (Arr ta _) = ta

getFstTyp :: Typ (A.Tpl tf ts) -> Typ tf  
getFstTyp (Tpl tf _) = tf

getSndTyp :: Typ (A.Tpl tf ts) -> Typ ts  
getSndTyp (Tpl _  ts) = ts

getAryTyp :: Typ (A.Ary ta) -> Typ ta  
getAryTyp (Ary ta) = ta