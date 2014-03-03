module Evaluation.Variable where

import Evaluation 
import qualified Variable              as G
import qualified Singleton.Environment as G
import Singleton

type instance Val (G.Var r t) = Trm t
type instance Env (G.Var r t) = Trm r 

instance Evl (G.Var r t) where
  evl = (return .) . G.get