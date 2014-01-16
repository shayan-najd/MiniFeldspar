{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances
           , GADTs #-}
module Conversion.Environment where

import qualified Environment.ADT  as A
import qualified Environment.GADT as G

import Conversion
import SingletonEquality
import Existential

instance Cnv a (ExsSin b) => Cnv (A.Env a) (ExsSin (G.Env b)) where
  cnv []      = return (ExsSin G.Emp)
  cnv (t : r) = do ExsSin t' <- cnv t
                   ExsSin r' <- cnv r
                   return (ExsSin (t' `G.Ext` r'))
 
instance Cnv a b => Cnv (A.Env a) (A.Env b)  where
  cnv = mapM cnv
  
instance (EqlSin tf , e ~ e') => Cnv (G.Env tf e , A.Env (Exs0 tf)) e' where  
  cnv (G.Emp        , [])                = return ()
  cnv (t `G.Ext` ts , (Exs0 x tv) : vs)  = do ts' <- cnv (ts , vs)
                                              Rfl <- eqlSin t tv 
                                              return (x,ts')
  cnv (_            , _)                 = fail "Scope Error!"  
 