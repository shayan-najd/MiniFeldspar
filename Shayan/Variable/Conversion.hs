{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Variable.Conversion where

import qualified Variable.ADT as U
import qualified Variable.GADT as T
import qualified Variable.Existential as W

import qualified Environment.GADT as T
import qualified Environment.Existential as W
import Environment.Conversion ()

import qualified Type.Existential as W

import Conversion
   
instance Cnv a W.Typ => Cnv (U.Var , [a]) W.Var where
 cnv (U.Zro   , t : r) = do W.Env r' <- cnv r 
                            W.Typ t' <- cnv t  
                            return (W.Var T.Zro (t' `T.Ext` r') t')
 cnv (U.Suc x , t : r) = do W.Var x' r' tr <- cnv (x , r)
                            W.Typ t'       <- cnv t
                            return (W.Var (T.Suc x') (t' `T.Ext` r') tr)
 cnv (_       , [])    = fail "Impossible!"  
                         -- the redundant pattern checker cannot guess that
                         -- and instance of Var never lets the environment to
                         -- to be empty.
 
