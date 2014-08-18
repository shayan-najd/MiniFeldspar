module Environment.Conversion () where

import MyPrelude

import qualified Environment.Map    as EM
import qualified Environment.Plain  as EP
import qualified Environment.Typed  as ET
import qualified Environment.Scoped as ES

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import Conversion
import Type.Feldspar.Conversion ()
import Singleton

---------------------------------------------------------------------------------
-- Conversion from EM.Env
---------------------------------------------------------------------------------
instance Cnv (a , r) b =>
         Cnv (EM.Env x a , r) (EM.Env x b) where
  cnv (ee , rr) = let ?r = rr in
    mapM (\(x , y) -> do y' <- cnvImp y
                         return (x , y')) ee

---------------------------------------------------------------------------------
-- Conversion from EP.Env
---------------------------------------------------------------------------------
instance Cnv (a , r) b =>
         Cnv (EP.Env a , r) (EP.Env b)  where
  cnv (ee , rr) = let ?r = rr in mapM cnvImp ee

instance Cnv (a , r) (ExsSin b) =>
         Cnv (EP.Env a , r) (ExsSin (ET.Env b)) where
  cnv (ee , rr) = let ?r = rr in case ee of
    []      -> return (ExsSin ET.Emp)
    (t : r) -> do ExsSin t' <- cnvImp t
                  ExsSin r' <- cnvImp r
                  return (ExsSin (ET.Ext t' r'))

---------------------------------------------------------------------------------
-- Conversion from ES.Env
---------------------------------------------------------------------------------
instance Cnv (ES.Env n t , r ) (EP.Env t) where
  cnv (ee , rr) = let ?r = rr in case ee of
    ES.Emp      -> pure []
    ES.Ext x xs -> (x :) <$@> xs

instance (Cnv (a , r) b , n ~ n') =>
         Cnv (ES.Env n a , r) (ES.Env n' b) where
  cnv (ee , rr) = let ?r = rr in mapM cnvImp ee

---------------------------------------------------------------------------------
-- Conversion from ES.Env
---------------------------------------------------------------------------------
instance n ~ Len r =>
         Cnv (ET.Env TFG.Typ r , rr) (ES.Env n TFA.Typ) where
  cnv (ee , rr) = let ?r = rr in case ee of
    ET.Emp      -> pure ES.Emp
    ET.Ext x xs -> ES.Ext <$@> x <*@> xs