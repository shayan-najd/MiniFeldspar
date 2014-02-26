{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs,TypeFamilies,FlexibleInstances, DataKinds, PolyKinds
           , TypeOperators, FlexibleContexts #-}
module Expression.Feldspar.MiniWellScoped where

import Prelude (Integer , Bool)
import Singleton
import qualified Variable               as V 
import qualified Type.Feldspar          as F
import qualified Singleton.TypeFeldspar as FG
import qualified Singleton.Environment  as G
 
type family Out (t :: F.Typ) :: F.Typ where 
  Out (F.Arr ta tb) = Out tb
  Out t             = t
    
type family Arg (t :: F.Typ) :: [F.Typ] where
  Arg (F.Arr ta tb) = ta ': Arg tb
  Arg t             = '[]
 
data Exp :: [F.Typ] -> F.Typ -> * where
  ConI  :: Integer -> Exp r F.Int
  ConB  :: Bool -> Exp r F.Bol
  AppV  :: FG.Typ t ->
           V.Var r t -> G.Env (Exp r) (Arg t) -> Exp r (Out t)
  Cnd   :: Exp r F.Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: FG.Typ t -> (Exp r t -> Exp r F.Bol) -> (Exp r t -> Exp r t) -> 
           Exp r t -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (F.Tpl tf ts)
  Fst   :: Exp r (F.Tpl tf ts) -> Exp r tf
  Snd   :: Exp r (F.Tpl tf ts) -> Exp r ts
  Ary   :: Exp r F.Int -> (Exp r F.Int -> Exp r t) -> Exp r (F.Ary t)
  Len   :: Exp r (F.Ary t) -> Exp r F.Int
  Ind   :: Exp r (F.Ary t) -> Exp r F.Int -> Exp r t
  Let   :: FG.Typ tl -> Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb

appV :: HasSin FG.Typ t => V.Var r t -> G.Env (Exp r) (Arg t) -> Exp r (Out t)
appV = AppV sin

whl ::  HasSin FG.Typ t => (Exp r t -> Exp r F.Bol) -> (Exp r t -> Exp r t) -> 
        Exp r t -> Exp r t
whl = Whl sin
  