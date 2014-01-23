{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs
           , ScopedTypeVariables, ImplicitParams #-}
module Conversion.Type.Feldspar where

import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Type.Feldspar.GADT                as FT
import qualified Type.Herbrand                     as H

import qualified Variable.GADT                     as G

import Data.Nat.GADT 
import Data.Vector
import Conversion
import Existential

type ExsTyp = ExsSin FT.Typ

type EnvFAMH = ( Zro                -- Int ~> 0
               , (Zro               -- Bol ~> 1 
                 , (Suc (Suc Zro)   -- Arr ~> 2 
                   , (Suc (Suc Zro) -- Tpl ~> 3  
                     , (Suc Zro     -- Ary ~> 4    
                       , ())))))  
               
---------------------------------------------------------------------------------
--  Conversion from FAS.Typ
---------------------------------------------------------------------------------
instance Cnv FAS.Typ ExsTyp where  
  cnv FAS.Int         = return (ExsSin FT.Int)
  cnv FAS.Bol         = return (ExsSin FT.Bol)
  cnv (FAS.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (FT.Arr ta' tr'))
  cnv (FAS.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (FT.Tpl tf' ts'))
  cnv (FAS.Ary t)     = do ExsSin t' <- cnv t
                           return (ExsSin (FT.Ary t'))

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
instance Cnv FAM.Typ (H.Typ EnvFAMH) where
  cnv FAM.Int         = return (H.App (G.Zro) Nil)
  cnv FAM.Bol         = return (H.App (G.Suc G.Zro) Nil)
  cnv (FAM.Arr ta tb) = do ta' <- cnv ta
                           tb' <- cnv tb
                           return (H.App (G.Suc (G.Suc G.Zro)) 
                                   (ta' ::: (tb' ::: Nil)))
  cnv (FAM.Tpl ta tb) = do ta' <- cnv ta
                           tb' <- cnv tb
                           return (H.App
                                   (G.Suc (G.Suc (G.Suc G.Zro))) 
                                   (ta' ::: (tb' ::: Nil)))
  cnv (FAM.Ary t)     = do t' <- cnv t
                           return (H.App 
                                   (G.Suc (G.Suc (G.Suc (G.Suc G.Zro))))
                                   (t' ::: Nil))
  cnv (FAM.Mta i)     = return (H.Mta i)                             

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
instance Cnv (FT.Typ a) FAS.Typ where
  cnv FT.Int         = return FAS.Int
  cnv FT.Bol         = return FAS.Bol
  cnv (FT.Arr ta tb) = do ta' <- cnv ta
                          tb' <- cnv tb
                          return (FAS.Arr ta' tb')
  cnv (FT.Tpl tf ts) = do tf' <- cnv tf
                          ts' <- cnv ts
                          return (FAS.Tpl tf' ts')
  cnv (FT.Ary t)     = do t' <- cnv t
                          return (FAS.Ary t')
 
instance Cnv (FT.Typ a) FAM.Typ where
  cnv FT.Int         = return FAM.Int
  cnv FT.Bol         = return FAM.Bol
  cnv (FT.Arr ta tb) = do ta' <- cnv ta
                          tb' <- cnv tb
                          return (FAM.Arr ta' tb')
  cnv (FT.Tpl tf ts) = do tf' <- cnv tf
                          ts' <- cnv ts
                          return (FAM.Tpl tf' ts')
  cnv (FT.Ary t)     = do t' <- cnv t
                          return (FAM.Ary t')

instance a ~ a' => Cnv (FT.Typ a) (FT.Typ a') where
  cnv = return . id

---------------------------------------------------------------------------------
--  Conversion from H.Typ
---------------------------------------------------------------------------------
instance Cnv (H.Typ EnvFAMH) FAM.Typ where
  cnv = cnv' 
    where
      cnv' th = case th of 
        H.Mta i               -> FAM.Mta <$> pure i
        H.App G.Zro _         -> pure FAM.Int
        H.App (G.Suc G.Zro) _ -> pure FAM.Bol
        H.App (G.Suc (G.Suc G.Zro)) (ta ::: (tb ::: Nil))
                              -> FAM.Arr <$@> ta <*@> tb
        H.App (G.Suc (G.Suc (G.Suc G.Zro))) (tf ::: (ts ::: Nil))
                              -> FAM.Tpl <$@> tf <*@> ts
        H.App (G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))) (t ::: Nil)
                              -> FAM.Ary <$@> t    
        _                     -> fail "Type Error!"  
        where ?cnv = cnv'
              