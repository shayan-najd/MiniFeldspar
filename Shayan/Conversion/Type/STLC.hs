{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs #-}
module Conversion.Type.STLC where

import qualified Type.STLC.ADTSimple as AS
import qualified Type.STLC.ADTWithMetavariable as AM
import qualified Type.STLC.GADT as T
import qualified Type.Herbrand as H
import qualified Variable.GADT as G
import Data.Vector
import Data.Nat.GADT
import Conversion
import ErrorMonad
import Existential

instance Cnv AS.Typ (ExsSin T.Typ) where
  cnv AS.Int         = return (ExsSin T.Int)
  cnv (AS.Arr ta tr) = do ExsSin ta' <- cnv ta
                          ExsSin tr' <- cnv tr
                          return (ExsSin (T.Arr ta' tr'))

type EnvAMH = (Zro            -- Int has no  argument
              ,(Suc (Suc Zro),())) -- Arr has two arguments
instance Cnv AM.Typ (H.Typ EnvAMH) where
  cnv AM.Int         = return (H.App Zro G.Zro Nil)
  cnv (AM.Arr ta tr) = do ta' <- cnv ta
                          tr' <- cnv tr
                          return (H.App (Suc (Suc Zro)) (G.Suc G.Zro) 
                                  (ta' ::: (tr' ::: Nil)) )
  cnv (AM.Mta i)     = return (H.Mta i)                           
 
instance Cnv (H.Typ EnvAMH) AM.Typ where
  cnv (H.App Zro G.Zro Nil)  = return AM.Int
  cnv (H.App (Suc (Suc Zro)) (G.Suc G.Zro) 
       (ta ::: (tr ::: Nil)) ) = do ta' <- cnv ta
                                    tr' <- cnv tr
                                    return (AM.Arr ta' tr')
  cnv (H.App _ _  _)  = fail "Type Error!"
  cnv (H.Mta i)     = return (AM.Mta i)                           


instance Cnv AM.Typ (ExsSin T.Typ) where
  cnv t = do t' <- cnv t :: ErrM AS.Typ
             cnv t' 
  {-
  cnv AM.Int         = return (W.Typ T.Int)
  cnv (AM.Arr ta tr) = do W.Typ ta' <- cnv ta
                          W.Typ tr' <- cnv tr
                          return (W.Typ (T.Arr ta' tr'))
  cnv _              = fail "Type Error!" 
  -}

instance Cnv AM.Typ AS.Typ where
  cnv AM.Int         = return AS.Int
  cnv (AM.Arr ta tr) = do ta' <- cnv ta
                          tr' <- cnv tr
                          return (AS.Arr ta' tr')
  cnv _              = fail "Type Error!"                          
  
instance Cnv AS.Typ AM.Typ where
  cnv AS.Int         = return AM.Int
  cnv (AS.Arr ta tr) = do ta' <- cnv ta
                          tr' <- cnv tr
                          return (AM.Arr ta' tr')