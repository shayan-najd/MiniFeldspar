module Conversion.Environment () where

import Prelude ()
import MyPrelude 

import qualified Environment.Plain  as EP
import qualified Environment.Map    as EM
import qualified Environment.Typed  as ET
import qualified Environment.Scoped as ES

import Environment.Typed (Len)

import qualified Type.Feldspar.ADT  as TFA 
import qualified Type.Feldspar.GADT as TFG 

import Conversion
import Conversion.Type.Feldspar ()

instance Cnv (a , ()) b => Cnv (EM.Env x a , ()) (EM.Env x b) where
  cnv = mapM (\(x , y) -> do y' <- cnv (y , ())
                             return (x , y')) . fst

instance Cnv (a , ()) (ExsSin b) => Cnv (EP.Env a , ()) (ExsSin (ET.Env b)) where
  cnv (ee , ()) = case ee of 
    []      -> return (ExsSin ET.Emp)
    (t : r) -> do ExsSin t' <- cnv (t , ())
                  ExsSin r' <- cnv (r , ())
                  return (ExsSin (ET.Ext t' r'))
                  
instance Cnv (a , ()) b => Cnv (EP.Env a , ()) (EP.Env b)  where
  cnv = mapM (flip (curry cnv) ()) . fst
 
instance Cnv (ES.Env n t , () ) (EP.Env t) where
  cnv (ee , ()) = let ?r = () in case ee of
    ES.Emp      -> pure []
    ES.Ext x xs -> (x :) <$@> xs
     
instance (Cnv (a , ()) b , n ~ n') => Cnv (ES.Env n a , ()) (ES.Env n' b) where  
  cnv = mapM (flip (curry cnv) ()) . fst

instance n ~ Len r => Cnv (ET.Env TFG.Typ r , ()) (ES.Env n TFA.Typ) where
  cnv (ee , ()) = let ?r = () in case ee of
    ET.Emp      -> pure ES.Emp
    ET.Ext x xs -> ES.Ext <$@> x <*@> xs