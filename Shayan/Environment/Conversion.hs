{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances , FlexibleContexts #-}
module Environment.Conversion where

import qualified Environment.ADT as U
import qualified Environment.GADT as T
import qualified Environment.Existential as W
import qualified Type.Existential as W

import Conversion

instance Cnv a W.Typ => Cnv (U.Env a) W.Env where
  cnv []      = return (W.Env T.Emp)
  cnv (t : r) = do W.Typ t' <- cnv t
                   W.Env r' <- cnv r
                   return (W.Env (t' `T.Ext` r'))
                                      
instance Cnv a b => Cnv (U.Env a) (U.Env b)  where
  cnv = mapM cnv