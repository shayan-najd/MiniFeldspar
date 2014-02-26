{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs
           , ScopedTypeVariables, ImplicitParams, DataKinds, PolyKinds 
           , NoMonomorphismRestriction #-}
module Conversion.Type.Feldspar where

import qualified Type.Feldspar                     as FAS
import qualified Singleton.TypeFeldspar            as FG
import qualified Type.Herbrand                     as H
import qualified Variable                          as G

import Data.Vector
import Conversion
import Existential
import ErrorMonad

type ExsTyp = ExsSin FG.Typ                
---------------------------------------------------------------------------------
--  Conversion from FAS.Typ
---------------------------------------------------------------------------------
instance Cnv FAS.Typ ExsTyp where  
  cnv FAS.Int         = return (ExsSin FG.Int)
  cnv FAS.Bol         = return (ExsSin FG.Bol)
  cnv (FAS.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (FG.Arr ta' tr'))
  cnv (FAS.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (FG.Tpl tf' ts'))
  cnv (FAS.Ary t)     = do ExsSin t' <- cnv t
                           return (ExsSin (FG.Ary t'))
                            
instance Cnv FAS.Typ (H.Typ (H.EnvFld '[])) where
  cnv th = case th of 
        FAS.Int       -> pure (H.App G.Zro Nil)
        FAS.Arr ta tb -> do ta' <- cnv ta
                            tb' <- cnv tb 
                            return (H.App (G.Suc G.Zro) 
                                    (ta' ::: (tb' ::: Nil)))
        FAS.Bol       -> return (H.App (G.Suc (G.Suc G.Zro)) Nil)
        FAS.Tpl tf ts -> do tf' <- cnv tf
                            ts' <- cnv ts
                            return (H.App (G.Suc (G.Suc (G.Suc G.Zro))) 
                                    (tf' ::: (ts' ::: Nil)))
        FAS.Ary ta    -> do ta' <- cnv ta
                            return (H.App (G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))) 
                                    (ta' ::: Nil))
 
---------------------------------------------------------------------------------
--  Conversion from FG.Typ
---------------------------------------------------------------------------------
instance Cnv (FG.Typ a) FAS.Typ where
  cnv = cnv' 
    where
      cnv' :: FG.Typ t -> ErrM FAS.Typ
      cnv' tg = case tg of
        FG.Int         -> return FAS.Int
        FG.Bol         -> return FAS.Bol
        FG.Arr ta tb   -> do ta' <- cnv' ta
                             tb' <- cnv' tb
                             return (FAS.Arr ta' tb')
        FG.Tpl tf ts   -> do tf' <- cnv' tf
                             ts' <- cnv' ts
                             return (FAS.Tpl tf' ts')
        FG.Ary ta      -> do ta' <- cnv' ta
                             return (FAS.Ary ta')
---------------------------------------------------------------------------------
--  Conversion from H.Typ
---------------------------------------------------------------------------------
instance Cnv (H.Typ (H.EnvFld '[])) FAS.Typ where
  cnv = cnv'
    where
      cnv' :: (H.Typ (H.EnvFld '[])) -> ErrM FAS.Typ
      cnv' th = case th of 
        H.App G.Zro _         -> pure FAS.Int      
        H.App (G.Suc G.Zro) (ta ::: (tb ::: Nil))
                              -> FAS.Arr <$@> ta <*@> tb
        H.App (G.Suc (G.Suc G.Zro)) _ 
                              -> pure FAS.Bol
        H.App (G.Suc (G.Suc (G.Suc G.Zro))) (tf ::: (ts ::: Nil))
                              -> FAS.Tpl <$@> tf <*@> ts
        H.App (G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))) (t ::: Nil)
                              -> FAS.Ary <$@> t    
        _                     -> fail "Type Error!"  
        where ?cnv = cnv'
