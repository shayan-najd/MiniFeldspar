{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Type.Conversion where

import qualified Type.ADTSimple as AS
import qualified Type.ADTWithMetavariable as AM
import qualified Type.GADT as T
import qualified Type.Existential as W

import Conversion
import ErrorMonad

instance Cnv AS.Typ W.Typ where
  cnv AS.Int         = return (W.Typ T.Int)
  cnv (AS.Arr ta tr) = do W.Typ ta' <- cnv ta
                          W.Typ tr' <- cnv tr
                          return (W.Typ (T.Arr ta' tr'))

instance Cnv AM.Typ W.Typ where
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