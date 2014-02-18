{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Evaluation.Variable.GADT where

import Evaluation 
import Variable.GADT
import Singleton.Environment
import Singleton

instance Evl (Var r t) where
  type Val (Var r t) = Trm t
  type Env (Var r t) = Trm r 
  evl = (return .) . get