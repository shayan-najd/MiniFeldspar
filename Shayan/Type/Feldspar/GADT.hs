module Type.Feldspar.GADT where

import MyPrelude

import qualified Type.Feldspar.ADT as A

import qualified Environment.Typed as ET

import Singleton

data Typ :: A.Typ -> * where
  Int :: Typ A.Int
  Bol :: Typ A.Bol
  Flt :: Typ A.Flt
  Arr :: Typ ta -> Typ tb -> Typ (A.Arr ta tb)
  Tpl :: Typ tf -> Typ ts -> Typ (A.Tpl tf ts)
  Ary :: Typ t  -> Typ (A.Ary t)
  May :: Typ t  -> Typ (A.May t)
  Cmx :: Typ A.Cmx

type instance Trm A.Int         = Word32
type instance Trm A.Bol         = Bool
type instance Trm A.Flt         = Float
type instance Trm (A.Arr ta tb) = Trm ta -> Trm tb
type instance Trm (A.Ary ta)    = Array Word32 (Trm ta)
type instance Trm (A.Tpl tf ts) = (Trm tf , Trm ts)
type instance Trm (A.May ta)    = Maybe (Trm ta)
type instance Trm A.Cmx         = Complex Float

type instance RevTrm Word32             = A.Int
type instance RevTrm Bool               = A.Bol
type instance RevTrm Float              = A.Flt
type instance RevTrm (ta -> tb)         = A.Arr (RevTrm ta) (RevTrm tb)
type instance RevTrm (tf , ts)          = A.Tpl (RevTrm tf) (RevTrm ts)
type instance RevTrm (Array Word32 ta)  = A.Ary (RevTrm ta)
type instance RevTrm (Maybe ta)         = A.May (RevTrm ta)
type instance RevTrm (Complex Float)    = A.Cmx

instance HasSin Typ A.Int where
  sin = Int

instance HasSin Typ A.Bol where
  sin = Bol

instance HasSin Typ A.Flt where
  sin = Flt

instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (A.Arr ta tb) where
  sin = Arr sin sin

instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (A.Tpl tf ts) where
  sin = Tpl sin sin

instance HasSin Typ ta => HasSin Typ (A.Ary ta) where
  sin = Ary sin

instance HasSin Typ ta => HasSin Typ (A.May ta) where
  sin = May sin

instance HasSin Typ A.Cmx where
  sin = Cmx

instance EqlSin Typ where
  eqlSin Int         Int           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin Flt         Flt           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (Ary t)     (Ary t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin (May t)     (May t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin Cmx         Cmx           = return Rfl
  eqlSin _              _          = fail "Type Error!"

instance GetPrfHasSin Typ where
  getPrfHasSin t  = case t of
    Int       -> PrfHasSin
    Bol       -> PrfHasSin
    Flt       -> PrfHasSin
    Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Ary ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin
    May ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin
    Cmx       -> PrfHasSin

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

getPrfHasSinMay :: forall ta t. HasSin Typ (A.May ta) =>
                   t (A.May ta) -> PrfHasSin Typ ta
getPrfHasSinMay _ = case sin :: Typ (A.May ta) of
  May ta    -> getPrfHasSin ta

getPrfHasSinArrM :: HasSin Typ (A.Arr ta tb) =>
                    t (A.Arr ta tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: HasSin Typ (A.Tpl tf ts) =>
                    t (A.Tpl tf ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

getPrfHasSinAryM :: HasSin Typ (A.Ary ta) =>
                   t (A.Ary ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinAryM = return  . getPrfHasSinAry

getPrfHasSinMayM :: HasSin Typ (A.May ta) =>
                   t (A.May ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinMayM = return  . getPrfHasSinMay


type family Out (t :: A.Typ) :: A.Typ where
  Out (A.Arr ta tb) = Out tb
  Out t             = t

data EqlOut :: A.Typ -> A.Typ -> * where
  EqlOut :: EqlOut (Out t2) t2

eqlOut :: Typ t1 -> Typ t2 -> ErrM (EqlOut t1 t2)
eqlOut Int         Int           = return EqlOut
eqlOut Bol         Bol           = return EqlOut
eqlOut Flt         Flt           = return EqlOut
eqlOut t           (Arr _ tb)    = do EqlOut <- eqlOut t tb
                                      return EqlOut
eqlOut (Ary ta)    (Ary ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (May ta)    (May ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                      Rfl <- eqlSin ts ts'
                                      return EqlOut
eqlOut Cmx         Cmx           = return EqlOut
eqlOut _              _          = fail "Normalization Error!"


type family Arg (t :: A.Typ) :: [A.Typ] where
  Arg (A.Arr ta tb) = ta ': Arg tb
  Arg t             = '[]

data EqlArg :: [A.Typ] -> A.Typ -> * where
  EqlArg :: EqlArg (Arg t2) t2

eqlArg :: ET.Env Typ r -> Typ t -> ErrM (EqlArg r t)
eqlArg ET.Emp         Int          = return EqlArg
eqlArg ET.Emp         Bol          = return EqlArg
eqlArg ET.Emp         Flt          = return EqlArg
eqlArg (ET.Ext ta ts) (Arr ta' tb) = do Rfl    <- eqlSin ta ta'
                                        EqlArg <- eqlArg ts tb
                                        return EqlArg
eqlArg ET.Emp         (Ary _)      = return EqlArg
eqlArg ET.Emp         (May _)      = return EqlArg
eqlArg ET.Emp         (Tpl _ _)    = return EqlArg
eqlArg ET.Emp         Cmx          = return EqlArg
eqlArg _              _            = fail "Normalization Error!"

mapC :: r ~ Arg tt =>
        Typ tt -> (forall t. HasSin Typ t => tfa t -> tfb t) ->
        ET.Env tfa r -> ET.Env tfb r
mapC _              _ ET.Emp     = ET.Emp
mapC (Arr t ts) f (ET.Ext x xs)  = case getPrfHasSin t of
  PrfHasSin                     -> ET.Ext (f x) (mapC ts f xs)
mapC _              _ _          = impossible

mapMC :: (Monad m , r ~ Arg tt) =>
        Typ tt -> (forall t. HasSin Typ t => tfa t -> m (tfb t)) ->
        ET.Env tfa r -> m (ET.Env tfb r)
mapMC _              _ ET.Emp    = return (ET.Emp)
mapMC (Arr t ts) f (ET.Ext x xs) = case getPrfHasSin t of
  PrfHasSin -> do x'  <- f x
                  xs' <- mapMC ts f xs
                  return (ET.Ext x' xs')
mapMC _              _ _        = impossibleM

getArgTyp :: Typ (A.Arr ta tb) -> Typ ta
getArgTyp (Arr ta _) = ta

getBdyTyp :: Typ (A.Arr ta tb) -> Typ tb
getBdyTyp (Arr _ tb) = tb

getFstTyp :: Typ (A.Tpl tf ts) -> Typ tf
getFstTyp (Tpl tf _) = tf

getSndTyp :: Typ (A.Tpl tf ts) -> Typ ts
getSndTyp (Tpl _  ts) = ts

getAryTyp :: Typ (A.Ary ta) -> Typ ta
getAryTyp (Ary ta) = ta

getMayTyp :: Typ (A.May ta) -> Typ ta
getMayTyp (May ta) = ta