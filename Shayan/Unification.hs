{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Unification (Uni(..) , module H) where

import Variable.GADT 
import Data.Vector
import Type.Herbrand as H hiding (Typ(..))
  
-- A type class providing operations required for type unification.      
class Monad (Mnd t) => Uni t where 
  type Mnd t :: * -> *
  type TypCons t      
  typCon :: Var (TypCons t) n -> Vec n t -> Mnd t t
  eql    :: t -> t -> Mnd t ()
  eqlCon :: Var (TypCons t) n -> t -> Mnd t (Vec n t)