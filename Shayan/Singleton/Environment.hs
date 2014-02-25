{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, Rank2Types, GADTs 
           , TypeFamilies #-}
module Singleton.Environment where

import Singleton
import Prelude (error)
import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Variable.GADT

-- Environment (Singleton)
data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

type instance Trm '[]        = ()
type instance Trm (tf ': ts) = (Trm tf , Trm ts)

type instance RevTrm ()        = '[]                   
type instance RevTrm (tf , ts) = (RevTrm tf ': RevTrm ts)                    
                   
                   
-- Extraction of values from environment
get :: Var r t -> Trm r -> Trm t
get Zro     (x , _ ) = x
get (Suc n) (_ , xs) = get n xs

-- Extraction of values from environment with singletons
gets :: Var r t -> Env tf r -> tf t
gets Zro     (x `Ext` _ ) = x
gets (Suc n) (_ `Ext` xs) = gets n xs
gets _       Emp          = error "Impossible!" 
                            -- the redundant pattern checker cannot guess that
                            -- and instance of Var never lets the environment to
                            -- to be empty.

wkn :: (forall r t'. tf r t' -> tf (t ': r) t') -> 
       Env (tf re) r' -> Env (tf (t ': re)) r'
wkn _      Emp                    = Emp
wkn sucAll (Ext e Emp)          = Ext (sucAll e) Emp
wkn sucAll (Ext e es@(Ext _ _)) = Ext (sucAll e) (wkn sucAll es)


cnvGEnvtoGVar ::  Env tf r -> Env (Var r) r
cnvGEnvtoGVar Emp        = Emp
cnvGEnvtoGVar (Ext _ xs) = Ext Zro (wkn Suc (cnvGEnvtoGVar xs))

instance HasSin (Env tf) '[] where
  sin = Emp
  
instance (HasSin tf t , HasSin (Env tf) ts) => HasSin (Env tf) (t ': ts) where
  sin = Ext sin sin
  
map :: (forall t. tfa t -> tfb t) -> Env tfa r -> Env tfb r
map _ Emp        = Emp
map f (Ext x xs) = Ext (f x) (map f xs)   
  
mapM :: Applicative m => 
        (forall t. tfa t -> m (tfb t)) -> Env tfa r -> m (Env tfb r)
mapM _ Emp        = pure Emp
mapM f (Ext x xs) = Ext <$> f x <*> mapM f xs