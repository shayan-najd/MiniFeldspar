{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, ImplicitParams 
           , ScopedTypeVariables #-}
module Conversion.Type.STLC where

import qualified Type.STLC.ADTSimple as AS
import qualified Type.STLC.ADTWithMetavariable as AM
import qualified Type.STLC.GADT as T
import qualified Type.Herbrand as H
import qualified Variable.GADT as G
import Data.Vector
import Data.Nat.GADT
import Conversion
import Existential

---------------------------------------------------------------------------------
-- Conversion from AS.Typ
---------------------------------------------------------------------------------
instance Cnv AS.Typ (ExsSin T.Typ) where
  cnv AS.Int         = return (ExsSin T.Int)
  cnv (AS.Arr ta tr) = do ExsSin ta' <- cnv ta
                          ExsSin tr' <- cnv tr
                          return (ExsSin (T.Arr ta' tr'))

instance Cnv AS.Typ AM.Typ where
  cnv AS.Int         = return AM.Int
  cnv (AS.Arr ta tb) = let ?cnv = cnv in 
                       AM.Arr <$@> ta <*@> tb

instance Cnv AS.Typ AS.Typ where                          
  cnv = return

---------------------------------------------------------------------------------
-- Conversion from AM.Typ
---------------------------------------------------------------------------------
type EnvAMH = (Zro            -- Int has no  argument
              ,(Suc (Suc Zro),())) -- Arr has two arguments

instance Cnv AM.Typ (H.Typ EnvAMH) where
  cnv AM.Int         = return (H.App G.Zro Nil)
  cnv (AM.Arr ta tr) = do ta' <- cnv ta
                          tr' <- cnv tr
                          return (H.App (G.Suc G.Zro) 
                                  (ta' ::: tr' ::: Nil) )
  cnv (AM.Mta i)     = return (H.Mta i)                           
 
instance Cnv AM.Typ (ExsSin T.Typ) where
  cnv t = do t' :: AS.Typ <- cnv t
             cnv t' 
 
instance Cnv AM.Typ AS.Typ where
  cnv AM.Int         = return AS.Int
  cnv (AM.Arr ta tr) = let ?cnv = cnv in 
                       AS.Arr <$@> ta <*@> tr
  cnv _              = fail "Type Error!"                          
  
  
instance Cnv AM.Typ AM.Typ where                          
  cnv = return  
  
---------------------------------------------------------------------------------
-- Conversion from AM.Typ
---------------------------------------------------------------------------------
instance Cnv (H.Typ EnvAMH) AM.Typ where
  cnv (H.App G.Zro Nil)  = pure AM.Int
  cnv (H.App (G.Suc G.Zro) (ta ::: tr ::: Nil)) 
                         = let ?cnv = cnv in 
                            AM.Arr <$@> ta <*@> tr
  cnv (H.App _  _)       = fail "Type Error!"
  cnv (H.Mta i)          = AM.Mta <$> pure i                           
