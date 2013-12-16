{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Environment.GADT where

import Type.GADT
import Variable.GADT

-- Environment (Singleton)
data Env e where
  Emp :: Env ()
  Ext :: Typ t -> Env e -> Env (t , e)

-- Extraction of values from environment
get :: Var e t -> e -> t
get Zro     (x , _ ) = x
get (Suc n) (_ , xs) = get n xs

-- Extraction of values from environment with singletons
gets :: Var e t -> Env e -> Typ t
gets Zro     (x `Ext` _ ) = x
gets (Suc n) (_ `Ext` xs) = gets n xs
gets _       Emp          = error "Impossible!" 
                            -- the redundant pattern checker cannot guess that
                            -- and instance of Var never lets the environment to
                            -- to be empty.
