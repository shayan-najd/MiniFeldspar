{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts 
           , GADTs #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies #-}
module Singleton.TypeSTLC where

import Prelude hiding (sin)
import Singleton
import qualified Type.STLC as A

data Typ t where
  Int :: Typ A.Int
  Arr :: Typ ta -> Typ tb -> Typ (A.Arr ta tb)
  
type instance Trm A.Int         = Integer   
type instance Trm (A.Arr ta tb) = Trm ta -> Trm tb
  
type instance RevTrm Integer    = A.Int 
type instance RevTrm (ta -> tb) = RevTrm ta `A.Arr` RevTrm tb


instance HasSin Typ A.Int where
  sin = Int 
 
instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (A.Arr ta tb) where
  sin = Arr sin sin
    
instance Show (Typ t) where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
