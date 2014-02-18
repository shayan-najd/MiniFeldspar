{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs
           , ScopedTypeVariables, ImplicitParams, DataKinds, PolyKinds #-}
module Conversion.Type.Feldspar where

import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Singleton.TypeFeldspar            as FG
import qualified Type.Herbrand                     as H

import qualified Variable.GADT                     as G

import Data.Vector
import Conversion
import Existential
import Unification
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

instance Cnv FAS.Typ FAM.Typ where
  cnv = cnv'
    where 
      cnv' tas = case tas of
        FAS.Int       -> pure FAM.Int
        FAS.Bol       -> pure FAM.Bol
        FAS.Arr ta tb -> FAM.Arr <$@> ta <*@> tb
        FAS.Tpl tf ts -> FAM.Tpl <$@> tf <*@> ts                          
        FAS.Ary t     -> FAM.Ary <$@> t                            
        where ?cnv = cnv'                            
                           
instance Cnv FAS.Typ FAS.Typ where
  cnv = return . id

---------------------------------------------------------------------------------
--  Conversion from FAM.Typ
---------------------------------------------------------------------------------
instance Cnv FAM.Typ (H.Typ (EnvFld '[])) where
  cnv = cnv' 
     where 
       cnv' tam = case tam of 
         FAM.Int       -> pure int
         FAM.Bol       -> pure bol
         FAM.Arr ta tb -> arr <$@> ta <*@> tb
         FAM.Tpl tf ts -> tpl <$@> tf <*@> ts
         FAM.Ary ta    -> ary <$@> ta
         FAM.Mta i     -> H.Mta <$> pure i                             
         where ?cnv = cnv'

instance Cnv FAM.Typ ExsTyp where
  cnv t = do t' :: FAS.Typ <- cnv t 
             cnv t' 

instance Cnv FAM.Typ FAS.Typ where
  cnv = cnv'
    where 
      cnv' tas = case tas of
        FAM.Int       -> pure FAS.Int
        FAM.Bol       -> pure FAS.Bol
        FAM.Arr ta tb -> FAS.Arr <$@> ta <*@> tb
        FAM.Tpl tf ts -> FAS.Tpl <$@> tf <*@> ts                          
        FAM.Ary t     -> FAS.Ary <$@> t                            
        _             -> fail "Type Error!"                          
        where ?cnv = cnv'
              
instance Cnv FAM.Typ FAM.Typ where
  cnv = return . id

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
 
instance Cnv (FG.Typ a) FAM.Typ where
  cnv FG.Int         = return FAM.Int
  cnv FG.Bol         = return FAM.Bol
  cnv (FG.Arr ta tb) = do ta' <- cnv ta
                          tb' <- cnv tb
                          return (FAM.Arr ta' tb')
  cnv (FG.Tpl tf ts) = do tf' <- cnv tf
                          ts' <- cnv ts
                          return (FAM.Tpl tf' ts')
  cnv (FG.Ary t)     = do t' <- cnv t
                          return (FAM.Ary t')

instance a ~ a' => Cnv (FG.Typ a) (FG.Typ a') where
  cnv = return . id

---------------------------------------------------------------------------------
--  Conversion from H.Typ
---------------------------------------------------------------------------------
instance Cnv (H.Typ (EnvFld '[])) FAM.Typ where
  cnv = cnv' 
    where
      cnv' th = case th of 
        H.Mta i               -> FAM.Mta <$> pure i
        H.App G.Zro _         -> pure FAM.Int      
        H.App (G.Suc G.Zro) (ta ::: (tb ::: Nil))
                              -> FAM.Arr <$@> ta <*@> tb
        H.App (G.Suc (G.Suc G.Zro)) _ 
                              -> pure FAM.Bol
        H.App (G.Suc (G.Suc (G.Suc G.Zro))) (tf ::: (ts ::: Nil))
                              -> FAM.Tpl <$@> tf <*@> ts
        H.App (G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))) (t ::: Nil)
                              -> FAM.Ary <$@> t    
        _                     -> fail "Type Error!"  
        where ?cnv = cnv'
              