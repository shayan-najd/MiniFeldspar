{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Unification where

-- A type class providing operations required for type unification.      
class Monad (Mnd t) => Uni t where 
  type Mnd t :: * -> *
  tCon   :: String -> [t] -> Mnd t t
  eql    :: t -> t -> Mnd t ()
  eqlCon :: String -> t -> Mnd t [t]