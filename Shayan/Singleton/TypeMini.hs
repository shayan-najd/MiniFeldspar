{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts
  , GADTs #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds,TypeFamilies #-}
module Singleton.TypeMini where

import Prelude hiding (sin)
import Singleton
import qualified Type.Mini as A
import Data.Array
  
data Typ t where
  Int :: Typ A.Int 
  Bol :: Typ A.Bol 
  Tpl :: Typ tf -> Typ ts -> Typ (A.Tpl tf ts)
  Ary :: Typ t  -> Typ (A.Ary t)

type instance Trm A.Int         = Integer
type instance Trm A.Bol         = Bool
type instance Trm (A.Tpl tf ts) = (Trm tf , Trm ts)
type instance Trm (A.Ary ta)    = Array Integer (Trm ta)

type instance RevTrm Integer            = A.Int 
type instance RevTrm Bool               = A.Bol
type instance RevTrm (tf , ts)          = A.Tpl (RevTrm tf) (RevTrm ts) 
type instance RevTrm (Array Integer ta) = A.Ary (RevTrm ta)

instance HasSin Typ A.Int where
  sin = Int 
 
instance HasSin Typ A.Bol where
  sin = Bol 

instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (A.Tpl tf ts) where
  sin = Tpl sin sin  
  
instance HasSin Typ ta => HasSin Typ (A.Ary ta) where
  sin = Ary sin  
  
instance Show (Typ t) where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t  