{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts
  , GADTs #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds,TypeFamilies #-}
module Singleton.TypeFeldspar where

import Prelude hiding (sin)
import Singleton
import qualified Type.Feldspar as A
import Data.Array
  
data Typ t where
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
  
instance Show (Typ t) where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t  
  
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
  