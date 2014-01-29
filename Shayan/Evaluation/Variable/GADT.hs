{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Evaluation.Variable.GADT where

import Evaluation 
import Variable.GADT
import Environment.GADT 

instance Evl (Var r t) where
  type Val (Var r t) = t
  type Env (Var r t) = r 
  evl = (return .) . get