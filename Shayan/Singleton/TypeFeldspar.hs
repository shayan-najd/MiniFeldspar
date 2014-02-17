{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts
  , GADTs #-}
module Singleton.TypeFeldspar where

import Prelude hiding (sin)
import Singleton
import Data.Array (Array)
 
data Typ t where
  Int :: Typ Integer
  Bol :: Typ Bool 
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  Tpl :: Typ tf -> Typ ts -> Typ (tf , ts)
  Ary :: Typ t  -> Typ (Array Integer t)

instance HasSin Typ Integer where
  sin = Int 
 
instance HasSin Typ Bool where
  sin = Bol 

instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (ta -> tb) where
  sin = Arr sin sin
  
instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (tf , ts) where
  sin = Tpl sin sin  
  
instance HasSin Typ ta => HasSin Typ (Array Integer ta) where
  sin = Ary sin  
  
instance Show (Typ t) where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t  