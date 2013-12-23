{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Type.Singleton where

import qualified Type.GADT as G
import Singleton
import ErrorMonad

instance Sig G.Typ Integer where
  sig Prx = return G.Int 
 
instance (Sig G.Typ ta , Sig G.Typ tb) => Sig G.Typ (ta -> tb) where
  sig Prx = do ta' <- sig (Prx :: Prx ta)
               tb' <- sig (Prx :: Prx tb)
               return (ta' `G.Arr` tb')
 
-- The remaining is only due to a bug in GHC (refer to GHCBUG.hs)
class SigTyp t where
  sigTyp :: Prx t -> ErrM (G.Typ t)

instance SigTyp Integer where
  sigTyp Prx = return G.Int 
 
instance (SigTyp ta , SigTyp tb) => SigTyp (ta -> tb) where
  sigTyp Prx = do ta' <- sigTyp (Prx :: Prx ta)
                  tb' <- sigTyp (Prx :: Prx tb)
                  return (ta' `G.Arr` tb')