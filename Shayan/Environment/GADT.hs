{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, Rank2Types #-}
module Environment.GADT where

import Variable.GADT

-- Environment (Singleton)
data Env tf e where
  Emp :: Env tf ()
  Ext :: tf t -> Env tf e -> Env tf (t , e)

-- Extraction of values from environment
get :: Var r t -> r -> t
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

wkn :: (forall r t'. tf r t' -> tf (t , r) t') -> 
       Env (tf re) r' -> Env (tf (t , re)) r'
wkn _      Emp                    = Emp
wkn sucAll (Ext e Emp)          = Ext (sucAll e) Emp
wkn sucAll (Ext e es@(Ext _ _)) = Ext (sucAll e) (wkn sucAll es)


cnvGEnvtoGVar ::  Env tf r -> Env (Var r) r
cnvGEnvtoGVar Emp        = Emp
cnvGEnvtoGVar (Ext _ xs) = Ext Zro (wkn Suc (cnvGEnvtoGVar xs))