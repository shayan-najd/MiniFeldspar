{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Environment.Singleton where

import qualified Type.GADT as G
import qualified Environment.GADT as G
import Type.Singleton 
import Singleton
import ErrorMonad

instance Sig G.Env () where
  sig Prx = return G.Emp
  
instance (Sig G.Typ t , Sig G.Env ts) => Sig G.Env (t , ts) where
  sig Prx = do t' <- sig (Prx :: Prx t)
               e' <- sig (Prx :: Prx ts)
               return (t' `G.Ext` e')

-- The remaining is only due to a bug in GHC (refer to GHCBUG.hs)
class SigEnv t where
  sigEnv :: Prx t -> ErrM (G.Env t)

instance SigEnv () where
  sigEnv Prx = return G.Emp
  
instance (SigTyp t , SigEnv ts) => SigEnv (t , ts) where
  sigEnv Prx = do t' <- sigTyp (Prx :: Prx t)
                  e' <- sigEnv (Prx :: Prx ts)
                  return (t' `G.Ext` e')
