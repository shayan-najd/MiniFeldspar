module Conversion.Variable () where

import Prelude ()
import MyPrelude 

import qualified Variable.Plain     as VP
import qualified Variable.Scoped    as VS
import qualified Variable.Typed     as VT 

import qualified Environment.Map    as EM
import qualified Environment.Plain  as EP
import qualified Environment.Scoped as ES
import qualified Environment.Typed  as ET
 
import qualified Nat.GADT           as NG

import Conversion
import Conversion.Environment ()
import Conversion.Nat         ()

import Singleton

instance Cnv (VT.Var r t , ()) VP.Nat where                     
  cnv (ee , ()) = let ?r = () in case ee of
    VT.Zro   -> return VP.Zro
    VT.Suc v -> VP.Suc <$@> v

instance Eq x => 
         Cnv (x          , EM.Env x  t) t      where
  cnv (x , r) = EM.get x r 

instance Cnv (VP.Var     , EP.Env    t) t      where
  cnv (x , r) = EP.get x r

instance Cnv (VS.Var n   , ES.Env n  t) t      where
  cnv (x , r) = return (ES.get x r)
 
instance Cnv (VT.Var r t , ET.Env tf r) (tf t) where
  cnv (x , r) = return (ET.get x r)

instance Cnv  (VP.Var , ())             VP.Var         where  
  cnv = pure . fst

instance n ~ n' => 
         Cnv (VS.Var n , ())            (VS.Var n')    where
  cnv = pure . fst
  
instance (n ~ n' , t ~ t') => 
         Cnv (VT.Var n t , ())          (VT.Var n' t') where
  cnv = pure . fst
  
instance Cnv (a , ()) (ExsSin b) => 
         Cnv (VP.Var , EP.Env a) (Exs2 VT.Var (ET.Env b) b) where
  cnv (VP.Zro   , t : r) = do ExsSin r' <- cnv (r , ()) 
                              ExsSin t' <- cnv (t , ()) 
                              return (Exs2 VT.Zro      (ET.Ext t' r') t')
  cnv (VP.Suc x , t : r) = do Exs2 x' r' tr <- cnv (x , r)
                              ExsSin t'     <- cnv (t , ())
                              return (Exs2 (VT.Suc x') (ET.Ext t' r') tr)
  cnv _                  = fail "Impossible!"  
 
instance Cnv (VP.Var , NG.Nat n) (VS.Var n) where
  cnv (VP.Zro   , NG.Suc  _) = return VS.Zro
  cnv (VP.Suc n , NG.Suc n') = VS.Suc <$> cnv (n , n')
  cnv _                      = fail "Impossible!"  
 
instance (n ~ ET.Len r , r ~ r' , EqlSin tf , HasSin tf t) => 
         Cnv (VS.Var n, ET.Env tf r) (VT.Var r' t)  where
  cnv (VS.Zro   , ET.Ext x _ ) = do Rfl <- eqlSin x (sin :: tf t) 
                                    return VT.Zro         
  cnv (VS.Suc n , ET.Ext _ xs) = VT.Suc <$> cnv (n , xs) 
  cnv _                        = fail "Impossible!" 
