{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Evaluation.STLC.GADTFirstOrder where

import Evaluation as E
import Evaluation.Variable.GADT ()
import Expression.STLC.GADTFirstOrder
import qualified Value.STLC.GADT as V
import Control.Applicative.Recursion 
import Singleton
import Singleton.TypeSTLC ()
import Singleton.Environment

instance Evl (Exp r t) where
  type Val (Exp r t) = Trm t
  type Env (Exp r t) = Trm r 
  evl egfo r = case egfo of
    Con i     -> V.con <$> pure i
    Var x     -> return (get x r)
    Abs _ eb  -> V.abs <$> pure (\ va -> evl eb (va , r))
    App ef ea -> V.app <$> evl ef r <*> evl ea r
    Add el er -> V.add <$> evl el r <*> evl er r
     
