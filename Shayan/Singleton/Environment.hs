{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, Rank2Types, GADTs 
           , TypeFamilies #-}
module Singleton.Environment where

import Singleton
import Prelude (error,return,fail)
import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Variable
import qualified Data.Nat as N
import qualified Singleton.Nat as V

-- Environment (Singleton)
data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

type instance Trm '[]        = ()
type instance Trm (tf ': ts) = (Trm tf , Trm ts)

type instance RevTrm ()        = '[]                   
type instance RevTrm (tf , ts) = (RevTrm tf ': RevTrm ts)                    
                                                     
instance HasSin (Env tf) '[] where
  sin = Emp
  
instance (HasSin tf t , HasSin (Env tf) ts) => HasSin (Env tf) (t ': ts) where
  sin = Ext sin sin
  
instance EqlSin tf => EqlSin (Env tf) where 
  eqlSin Emp       Emp         = return Rfl
  eqlSin (Ext t e) (Ext t' e') = do Rfl <- eqlSin e e'
                                    Rfl <- eqlSin t t'
                                    return Rfl
  eqlSin  _           _        = fail "Scope Error!"     
  
type family Len (l :: [k]) :: N.Nat where
  Len (x ': xs) = N.Suc (Len xs)
  Len '[]       = N.Zro     
 
type family Get (n :: N.Nat) (l :: [k]) :: k where
  Get N.Zro     (x ': xs) = x
  Get (N.Suc n) (x ': xs) = Get n xs
                                    
len :: Env ef r -> V.Nat (Len r)
len Emp        = V.Zro
len (Ext _ xs) = V.Suc (len xs)


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

  
  
map :: (forall t. tfa t -> tfb t) -> Env tfa r -> Env tfb r
map _ Emp        = Emp
map f (Ext x xs) = Ext (f x) (map f xs)   
  
mapM :: Applicative m => 
        (forall t. tfa t -> m (tfb t)) -> Env tfa r -> m (Env tfb r)
mapM _ Emp        = pure Emp
mapM f (Ext x xs) = Ext <$> f x <*> mapM f xs