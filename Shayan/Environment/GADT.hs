{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
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
