{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.TypeSTLC where

import Prelude hiding (sin)
import qualified Type.STLC.GADT as G
import Singleton

instance HasSin G.Typ Integer where
  sin = G.Int 
 
instance (HasSin G.Typ ta , HasSin G.Typ tb) => HasSin G.Typ (ta -> tb) where
  sin = G.Arr sin sin