{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.Environment where

import Environment.GADT 
import Singleton

import Prelude hiding (sin)
import Control.Applicative ((<$>),(<*>))

instance Sin (Env tf) () where
  sin = return Emp
  
instance (Sin tf t , Sin (Env tf) ts) => Sin (Env tf) (t , ts) where
  sin = Ext <$> sin <*> sin