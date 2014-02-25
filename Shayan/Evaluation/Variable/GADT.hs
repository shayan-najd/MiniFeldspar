{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Evaluation.Variable.GADT where

import Evaluation 
import Variable.GADT
import Singleton.Environment hiding (Env)
import Singleton

type instance Val (Var r t) = Trm t
type instance Env (Var r t) = Trm r 

instance Evl (Var r t) where
  evl = (return .) . get