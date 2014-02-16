{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.TypeFeldspar where

import Prelude hiding (sin)
import qualified Type.Feldspar.GADT as G
import Singleton
import Data.Array (Array)

instance HasSin G.Typ Integer where
  sin = G.Int 
 
instance HasSin G.Typ Bool where
  sin = G.Bol 

instance (HasSin G.Typ ta , HasSin G.Typ tb) => HasSin G.Typ (ta -> tb) where
  sin = G.Arr sin sin
  
instance (HasSin G.Typ tf , HasSin G.Typ ts) => HasSin G.Typ (tf , ts) where
  sin = G.Tpl sin sin  
  
instance HasSin G.Typ ta => HasSin G.Typ (Array Integer ta) where
  sin = G.Ary sin  