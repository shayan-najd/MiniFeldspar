{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Existential where 

data Exs0 tf where
  Exs0 :: t -> tf t -> Exs0 tf
  
data Exs1 c1 tf where
  Exs1 :: c1 t -> tf t -> Exs1 c1 tf
  
-- Special case for singletons 
data ExsSin st where -- Exs1 st st
  ExsSin :: st t -> ExsSin st

data Exs2 c2 tf1 tf2 where
  Exs2 :: c2 t1 t2 -> tf1 t1 -> tf2 t2 -> Exs2 c2 tf1 tf2
