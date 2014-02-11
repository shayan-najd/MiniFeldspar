{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.Environment where

import Environment.GADT 
import Singleton

import Prelude ()

instance Sin (Env tf) () where
  sin = Emp
  
instance (Sin tf t , Sin (Env tf) ts) => Sin (Env tf) (t , ts) where
  sin = Ext sin sin