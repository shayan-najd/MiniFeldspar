{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs
           , ScopedTypeVariables #-}
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

instance a ~ a' => Cnv (FT.Typ a) (FT.Typ a') where
  cnv = return . id

instance Cnv FAS.Typ FAS.Typ where
  cnv = return . id
 
instance Cnv FAM.Typ FAM.Typ where
  cnv = return . id

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
--  cnv FT.Any         = fail "Type Error!"                      


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
--  cnv FT.Any         = undefined

instance Cnv FAM.Typ (H.Typ EnvFAMH) where
  cnv FAM.Int         = return (H.App Zro (G.Zro) Nil)
  cnv FAM.Bol         = return (H.App Zro (G.Suc G.Zro) Nil)
  cnv (FAM.Arr ta tb) = do ta' <- cnv ta
                           tb' <- cnv tb
                           return (H.App (Suc (Suc Zro)) (G.Suc (G.Suc G.Zro)) 
                                   (ta' ::: (tb' ::: Nil)))
  cnv (FAM.Tpl ta tb) = do ta' <- cnv ta
                           tb' <- cnv tb
                           return (H.App (Suc (Suc Zro)) 
                                   (G.Suc (G.Suc (G.Suc G.Zro))) 
                                   (ta' ::: (tb' ::: Nil)))
  cnv (FAM.Ary t)     = do t' <- cnv t
                           return (H.App (Suc Zro)
                                   (G.Suc (G.Suc (G.Suc (G.Suc G.Zro))))
                                   (t' ::: Nil))
  cnv (FAM.Mta i)     = return (H.Mta i)                             
  
instance Cnv (H.Typ EnvFAMH) FAM.Typ where
  cnv (H.Mta i)                                         = return (FAM.Mta i)
  cnv (H.App _ G.Zro                                 _) = return FAM.Int
  cnv (H.App _ (G.Suc G.Zro)                         _) = return FAM.Bol
  cnv (H.App _ (G.Suc (G.Suc G.Zro)) (ta ::: (tb ::: Nil)))         = do 
    ta' <- cnv ta   
    tb' <- cnv tb
    return (FAM.Arr ta' tb')
  cnv (H.App _ (G.Suc (G.Suc (G.Suc G.Zro))) (tf ::: (ts ::: Nil))) = do  
    tf' <- cnv tf   
    ts' <- cnv ts
    return (FAM.Tpl tf' ts')
  cnv (H.App _ (G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))) (t ::: Nil))   = do 
    t' <- cnv t   
    return (FAM.Ary t')    
  cnv _ = fail "Type Error!"  
    
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

instance Cnv FAM.Typ ExsTyp where
  cnv t = do t' :: FAS.Typ <- cnv t 
             cnv t' 
  {-
  cnv AM.Int         = return (W.Typ T.Int)
  cnv (AM.Arr ta tr) = do W.Typ ta' <- cnv ta
                          W.Typ tr' <- cnv tr
                          return (W.Typ (T.Arr ta' tr'))
  cnv _              = fail "Type Error!" 
  -}

instance Cnv FAM.Typ FAS.Typ where
  cnv FAM.Int         = return FAS.Int
  cnv FAM.Bol         = return FAS.Bol
  cnv (FAM.Arr ta tr) = do ta' <- cnv ta
                           tr' <- cnv tr
                           return (FAS.Arr ta' tr')
  cnv (FAM.Tpl tf ts) = do tf' <- cnv tf
                           ts' <- cnv ts
                           return (FAS.Tpl tf' ts')                          
  cnv (FAM.Ary t)     = do t' <- cnv t
                           return (FAS.Ary t')
  cnv _               = fail "Type Error!"                          
  
instance Cnv FAS.Typ FAM.Typ where
  cnv FAS.Int         = return FAM.Int
  cnv FAS.Bol         = return FAM.Bol
  cnv (FAS.Arr ta tr) = do ta' <- cnv ta
                           tr' <- cnv tr
                           return (FAM.Arr ta' tr')
  cnv (FAS.Tpl tf ts) = do tf' <- cnv tf
                           ts' <- cnv ts
                           return (FAM.Tpl tf' ts')                          
  cnv (FAS.Ary t)     = do t' <- cnv t
                           return (FAM.Ary t')                            