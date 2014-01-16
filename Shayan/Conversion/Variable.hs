{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Conversion.Variable where

import qualified Variable.ADT     as A
import qualified Variable.GADT    as G
import qualified Environment.ADT  as A
import qualified Environment.GADT as G
import Conversion.Environment ()
import Existential
import Conversion
 
instance Cnv a (ExsSin b) => Cnv (A.Var , A.Env a) (Exs2 G.Var (G.Env b) b) where
 cnv (A.Zro   , t : r) = do ExsSin r' <- cnv r 
                            ExsSin t' <- cnv t  
                            return (Exs2 G.Zro (t' `G.Ext` r') t')
 cnv (A.Suc x , t : r) = do Exs2 x' r' tr <- cnv (x , r)
                            ExsSin t'     <- cnv t
                            return (Exs2 (G.Suc x') (t' `G.Ext` r') tr)
 cnv (_       , [])    = fail "Impossible!"  
 
