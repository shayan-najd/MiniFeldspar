{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts 
           , GADTs #-}
module Singleton.TypeSTLC where

import Prelude hiding (sin)
import Singleton

data Typ t where
  Int :: Typ Integer
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  
instance HasSin Typ Integer where
  sin = Int 
 
instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (ta -> tb) where
  sin = Arr sin sin
    
instance Show (Typ t) where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
