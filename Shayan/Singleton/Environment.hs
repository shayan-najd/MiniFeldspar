{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.Environment where

import Environment.GADT 
import Singleton

import Prelude ()

instance HasSin (Env tf) () where
  sin = Emp
  
instance (HasSin tf t , HasSin (Env tf) ts) => HasSin (Env tf) (t , ts) where
  sin = Ext sin sin